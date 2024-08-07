#' Create a summarised result with the number of people in observation during a specific interval of time.
#'
#' @param observationPeriod observation_period omop table.
#' @param unit Whether to stratify by "year" or by "month".
#' @param unitInterval Number of years or months to include within the time interval.
#' @param output Output format. It can be either the number of records ("records") that are in observation in the specific interval of time, the number of person-days ("person-days"), or both ("all").
#' @param ageGroup A list of age groups to stratify results by.
#' @param sex Boolean variable. Whether to stratify by sex (TRUE) or not (FALSE).
#'
#' @return A summarised_result object.
#'
#' @export
#'
summariseInObservation <- function(observationPeriod, unit = "year", unitInterval = 1, output = "records", ageGroup = NULL, sex = FALSE){

  # Initial checks ----
  assertClass(observationPeriod, "omop_table")

  x <- omopgenerics::tableName(observationPeriod)
  if (x != "observation_period") {
    cli::cli_abort(
      "Table name ({x}) is not observation_period, please provide a valid
      observation_period table"
    )
  }

  if(observationPeriod |> dplyr::tally() |> dplyr::pull("n") == 0){
    cli::cli_warn("observation_period table is empty. Returning an empty summarised result.")
    return(omopgenerics::emptySummarisedResult())
  }

  checkAgeGroup(ageGroup)

  if(missing(unit)){unit <- "year"}
  if(missing(unitInterval)){unitInterval <- 1}
  if(missing(ageGroup) | is.null(ageGroup)){ageGroup <- list("overall" = c(0,Inf))}else{ageGroup <- append(ageGroup, list("overall" = c(0, Inf)))}

  checkUnit(unit)
  checkUnitInterval(unitInterval)
  assertLogical(sex, length = 1)
  checkOutput(output)

  # Create initial variables ----
  cdm <- omopgenerics::cdmReference(observationPeriod)
  observationPeriod <- addStrataVariables(cdm, ageGroup, sex)

  # Observation period ----
  name <- "observation_period"
  start_date_name <- startDate(name)
  end_date_name   <- endDate(name)

  interval <- getIntervalTibbleForObservation(observationPeriod, start_date_name, end_date_name, unit, unitInterval)

  # Insert interval table to the cdm ----
  cdm <- cdm |>
    omopgenerics::insertTable(name = "interval", table = interval)

  # Calculate denominator ----
  denominator <- cdm |> getDenominator(output)

  # Count records ----
  result <- observationPeriod |>
    countRecords(cdm, start_date_name, end_date_name, unit, output)

  # Add category sex overall
  result <- addSexOverall(result, sex)

  # Create summarisedResult
  result <- createSummarisedResultObservationPeriod(result, observationPeriod, name, denominator, unit, unitInterval)

  omopgenerics::dropTable(cdm = cdm, name = "interval")
  return(result)
}

getDenominator <- function(cdm, output){
  if(output == "records"){
    tibble::tibble(
      "denominator" = c(cdm[["person"]] |>
                          dplyr::ungroup() |>
                          dplyr::select("person_id") |>
                          dplyr::summarise("n" = dplyr::n()) |>
                          dplyr::pull("n")),
      "variable_name" = "records")
  }else if(output == "person-days"){
    y <- cdm[["observation_period"]] |>
      dplyr::ungroup() |>
      dplyr::inner_join(cdm[["person"]] |> dplyr::select("person_id"), by = "person_id") %>%
      dplyr::mutate(n = !!CDMConnector::datediff("observation_period_start_date", "observation_period_end_date",interval = "day")+1) |>
      dplyr::summarise("n" = sum(.data$n, na.rm = TRUE)) |>
      dplyr::pull("n")

    tibble::tibble(
      "denominator" = y,
      "variable_name" = "person-days")

  }else if(output == "all"){
    y <- cdm[["observation_period"]] |>
      dplyr::ungroup() |>
      dplyr::inner_join(cdm[["person"]] |> dplyr::select("person_id"), by = "person_id") %>%
      dplyr::mutate(n = !!CDMConnector::datediff("observation_period_start_date", "observation_period_end_date",interval = "day")+1) |>
      dplyr::summarise("n" = sum(.data$n, na.rm = TRUE)) |>
      dplyr::pull("n")

    tibble::tibble(
      "denominator" = c(cdm[["person"]] |>
                          dplyr::ungroup() |>
                          dplyr::select("person_id") |>
                          dplyr::summarise("n" = dplyr::n()) |>
                          dplyr::pull("n"),
                        y
                        ),
      "variable_name" = c("records","person-days"))
  }
}

getIntervalTibbleForObservation <- function(omopTable, start_date_name, end_date_name, unit, unitInterval){
  startDate <- getOmopTableStartDate(omopTable, start_date_name)
  endDate   <- getOmopTableEndDate(omopTable, end_date_name)

  tibble::tibble(
    "group" = seq.Date(startDate, endDate, .env$unit)
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate("interval" = max(which(
      .data$group >= seq.Date(from = startDate, to = endDate, by = paste(.env$unitInterval, .env$unit))
    ),
    na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$interval) |>
    dplyr::mutate(
      "interval_start_date" = min(.data$group),
      "interval_end_date"   = dplyr::if_else(.env$unit == "year",
                                             clock::add_years(min(.data$group),.env$unitInterval)-1,
                                             clock::add_months(min(.data$group),.env$unitInterval)-1)
    ) |>
    dplyr::mutate(
      "interval_start_date" = as.Date(.data$interval_start_date),
      "interval_end_date" = as.Date(.data$interval_end_date)
    ) |>
    dplyr::mutate(
      "interval_group" = paste(.data$interval_start_date,"to",.data$interval_end_date)
    ) |>
    dplyr::ungroup() |>
    dplyr::select("interval_start_date", "interval_end_date", "interval_group") |>
    dplyr::distinct()
}

countRecords <- function(observationPeriod, cdm, start_date_name, end_date_name, unit, output){
  tablePrefix <- omopgenerics::tmpPrefix()

  if(output == "person-days" | output == "all"){
    x <- cdm[["interval"]] |>
      dplyr::cross_join(
        observationPeriod |>
          dplyr::select("start_date" = "observation_period_start_date",
                        "end_date"   = "observation_period_end_date",
                        "age_group", "sex","person_id")
      ) |>
      dplyr::filter((.data$start_date < .data$interval_start_date & .data$end_date >= .data$interval_start_date) |
                      (.data$start_date >= .data$interval_start_date & .data$start_date <= .data$interval_end_date)) %>%
      dplyr::mutate(start_date = pmax(.data$interval_start_date, .data$start_date, na.rm = TRUE)) |>
      dplyr::mutate(end_date   = pmin(.data$interval_end_date, .data$end_date, na.rm = TRUE)) |>
      dplyr::compute(temporary = FALSE, name = tablePrefix)

    personDays <- x %>%
      dplyr::mutate(estimate_value = !!CDMConnector::datediff("start_date","end_date", interval = "day")+1) |>
      dplyr::group_by(.data$interval_group, .data$sex, .data$age_group) |>
      dplyr::summarise(estimate_value = sum(.data$estimate_value, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(variable_name = "person-days") |>
      dplyr::collect()
  }else{
    personDays <- createEmptyIntervalTable()
  }

if(output == "records" | output == "all"){
    x <- observationPeriod |>
      dplyr::mutate("start_date" = as.Date(paste0(clock::get_year(.data[[start_date_name]]),"/",clock::get_month(.data[[start_date_name]]),"/01"))) |>
      dplyr::mutate("end_date"   = as.Date(paste0(clock::get_year(.data[[end_date_name]]),"/",clock::get_month(.data[[end_date_name]]),"/01"))) |>
      dplyr::group_by(.data$start_date, .data$end_date, .data$age_group, .data$sex) |>
      dplyr::summarise(estimate_value = dplyr::n(), .groups = "drop") |>
      dplyr::compute(temporary = FALSE, name = tablePrefix)

    records <- cdm[["interval"]] |>
      dplyr::cross_join(x) |>
      dplyr::filter((.data$start_date < .data$interval_start_date & .data$end_date >= .data$interval_start_date) |
                      (.data$start_date >= .data$interval_start_date & .data$start_date <= .data$interval_end_date)) |>
      dplyr::group_by(.data$interval_group, .data$age_group, .data$sex) |>
      dplyr::summarise(estimate_value = sum(.data$estimate_value, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(variable_name = "records") |>
      dplyr::collect()
  }else{
    records <- createEmptyIntervalTable()
  }

  x <- personDays |>
    rbind(records) |>
    dplyr::arrange(.data$interval_group) |>
    dplyr::rename("time_interval" = "interval_group")

  omopgenerics::dropTable(cdm = cdm, name = c(dplyr::starts_with(tablePrefix)))

  return(x)
}

createSummarisedResultObservationPeriod <- function(result, observationPeriod, name, denominator, unit, unitInterval){
  result <- result |>
    dplyr::mutate("estimate_value" = as.character(.data$estimate_value)) |>
    dplyr::rename("variable_level" = "time_interval") |>
    visOmopResults::uniteStrata(cols = c("sex", "age_group")) |>
    dplyr::mutate(
      "result_id" = as.integer(1),
      "cdm_name" = omopgenerics::cdmName(omopgenerics::cdmReference(observationPeriod)),
      "group_name"  = "omop_table",
      "group_level" = name,
      "estimate_name" = "count",
      "estimate_type" = "integer",
      "additional_name" = "overall",
      "additional_level" = "overall"
    )

  result <- result |>
    rbind(result) |>
    dplyr::group_by(.data$variable_level, .data$strata_level, .data$variable_name) |>
    dplyr::mutate(estimate_type = dplyr::if_else(dplyr::row_number() == 2, "percentage", .data$estimate_type)) |>
    dplyr::inner_join(denominator, by = "variable_name") |>
    dplyr::mutate(estimate_value = dplyr::if_else(.data$estimate_type == "percentage", as.character(as.numeric(.data$estimate_value)/denominator*100), .data$estimate_value)) |>
    dplyr::select(-c("denominator")) |>
    dplyr::mutate(estimate_name = dplyr::if_else(.data$estimate_type == "percentage", "percentage", .data$estimate_name)) |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      "result_id" = 1L,
      "result_type" = "summarised_observation_period",
      "package_name" = "OmopSketch",
      "package_version" = as.character(utils::packageVersion("OmopSketch")),
      "unit" = .env$unit,
      "unitInterval" = .env$unitInterval
    ))

  return(result)
}

addStrataVariables <- function(cdm, ageGroup, sex){
  cdm$omop_table <- suppressMessages(
    cdm |>
      CohortConstructor::demographicsCohort(name = "omop_table",
                                            sex = NULL,
                                            ageRange = ageGroup,
                                            minPriorObservation = NULL,
                                            minFutureObservation = NULL)
  )

  age_tibble <- dplyr::tibble(
    "age_range" = gsub(",","_",gsub("\\)","",gsub("c\\(","",gsub(" ","",ageGroup)))),
    "age_group" = names(ageGroup)
  )

  tablePrefix <-  omopgenerics::tmpPrefix()

  settings <- cdm$omop_table |>
    CDMConnector::settings() |>
    dplyr::inner_join(age_tibble, by = "age_range") |>
    dplyr::select("cohort_definition_id","age_group")

  cdm <- cdm |>
    omopgenerics::insertTable(name = tablePrefix, table = settings)

  observationPeriod <- cdm$omop_table |>
    dplyr::inner_join(cdm[[tablePrefix]], by = "cohort_definition_id") |>
    dplyr::rename("observation_period_start_date" = "cohort_start_date",
                  "observation_period_end_date"   = "cohort_end_date",
                  "person_id" = "subject_id") |>
    dplyr::select(-c("cohort_definition_id")) |>
    dplyr::inner_join(
      cdm[["person"]] |> dplyr::select("person_id"), by = "person_id"
    ) |>
    dplyr::compute(name = "observationPeriod", temporary = FALSE)

  if(sex){
    observationPeriod <- observationPeriod |> PatientProfiles::addSexQuery()
  }else{
    observationPeriod <- observationPeriod |> dplyr::mutate(sex = "overall")
  }

  CDMConnector::dropTable(cdm, name = tablePrefix)
  return(observationPeriod)
}

addSexOverall <- function(result, sex){
  if(sex){
    result <- result |> rbind(
      result |>
        dplyr::group_by(.data$age_group, .data$time_interval, .data$variable_name) |>
        dplyr::summarise(estimate_value = sum(.data$estimate_value, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(sex = "overall")
    )
  }
  return(result)
}

createEmptyIntervalTable <- function(){
  tibble::tibble(
    "interval_group" = as.character(),
    "sex" = as.character(),
    "age_group" = as.character(),
    "estimate_value" = as.double()
  )
}
