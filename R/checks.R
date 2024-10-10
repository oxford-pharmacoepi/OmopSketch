#' @noRd
checkUnit <- function(unit, call = parent.frame()){
  omopgenerics::assertCharacter(unit, length = 1, na = FALSE, null = FALSE, call = call)

  if(!unit %in% c("year","month")){
    cli::cli_abort("Unit argument {unit} is not valid. Valid options are either `year` or `month`.", call = call)
  }
}

#' @noRd
checkCategory <- function(category, overlap = FALSE, type = "numeric", call = parent.frame()) {
  omopgenerics::assertList(
    category,
    types = type, any.missing = FALSE, unique = TRUE,
    min.len = 1
  )

  if (is.null(names(category))) {
    names(category) <- rep("", length(category))
  }

  # check length
  category <- lapply(category, function(x) {
    if (length(x) == 1) {
      x <- c(x, x)
    } else if (length(x) > 2) {
      cli::cli_abort(
        paste0(
          "Categories should be formed by a lower bound and an upper bound, ",
          "no more than two elements should be provided."
        ),
        call. = FALSE
      )
    }
    invisible(x)
  })

  # check lower bound is smaller than upper bound
  checkLower <- unlist(lapply(category, function(x) {
    x[1] <= x[2]
  }))
  if (!(all(checkLower))) {
    cli::cli_abort("Lower bound should be equal or smaller than upper bound", call = call)
  }

  # built tibble
  result <- lapply(category, function(x, call = parent.frame()) {
    dplyr::tibble(lower_bound = x[1], upper_bound = x[2])
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(category_label = names(.env$category)) |>
    dplyr::mutate(category_label = dplyr::if_else(
      .data$category_label == "",
      dplyr::case_when(
        is.infinite(.data$lower_bound) & is.infinite(.data$upper_bound) ~ "any",
        is.infinite(.data$lower_bound) ~ paste(.data$upper_bound, "or below"),
        is.infinite(.data$upper_bound) ~ paste(.data$lower_bound, "or above"),
        TRUE ~ paste(.data$lower_bound, "to", .data$upper_bound)
      ),
      .data$category_label
    )) |>
    dplyr::arrange(.data$lower_bound)

  # check overlap
  if (!overlap) {
    if (nrow(result) > 1) {
      lower <- result$lower_bound[2:nrow(result)]
      upper <- result$upper_bound[1:(nrow(result) - 1)]
      if (!all(lower > upper)) {
        cli::cli_abort("There can not be overlap between categories", call = call)
      }
    }
  }

  invisible(result)
}

#' @noRd
checkOutput <- function(output, call = parent.frame()){
  omopgenerics::assertCharacter(output, call = call)

  for(i in output){
    if(!i %in% c("person-days","records")){
      cli::cli_abort("output argument is not valid. It must be either `person-days`, `records`, or c(`person-days`,`records`).", call = call())
    }
  }
}

#' @noRd
checkCountBy <- function(countBy, call = parent.frame()){
  omopgenerics::assertCharacter(countBy, call = call)

  for(i in countBy){
    if(!i %in% c("record","person")){
      cli::cli_abort("countBy argument is not valid. It must be either `record`, `person`, or c(`record`,`person`).", call = call)
    }
  }
}

#' @noRd
validateStudyPeriod <- function(cdm, studyPeriod, call = parent.frame()) {
  if(is.null(studyPeriod)) {
    studyPeriod <- c(NA,NA)
  }
  # First date checks
  if(!is.na(studyPeriod[1]) & !is.na(studyPeriod[2]) & studyPeriod[1] > studyPeriod[2]) {
    cli::cli_abort("The studyPeriod ends at a date earlier than the start provided.", call = call)
  }
  if(!is.na(studyPeriod[1]) & is.na(as.Date(studyPeriod[1], format="%d/%m/%Y")) && is.na(as.Date(studyPeriod[1], format="%Y-%m-%d"))) {
    cli::cli_abort("Please ensure that dates provided are in the correct format.", call = call)
  }
  if(!is.na(studyPeriod[2]) & is.na(as.Date(studyPeriod[2], format="%d/%m/%Y")) && is.na(as.Date(studyPeriod[2], format="%Y-%m-%d"))) {
    cli::cli_abort("Please ensure that dates provided are in the correct format.", call = call)
  }

  studyPeriod <- as.character(studyPeriod)
  omopgenerics::assertCharacter(studyPeriod, length = 2, na = TRUE)
  observationRange <- cdm$observation_period |>
    dplyr::summarise(minobs = min(.data$observation_period_start_date, na.rm = TRUE),
                     maxobs = max(.data$observation_period_end_date, na.rm = TRUE)) |>
    dplyr::collect()

  if(is.na(studyPeriod[1])){
    studyPeriod[1] <- observationRange |>
      dplyr::pull("minobs") |>
      as.character()
  } else {
    if(observationRange |>
       dplyr::pull("minobs") > studyPeriod[1]) {
      cli::cli_alert(paste0("The observation period in the cdm starts in ",observationRange |>
                              dplyr::pull("minobs")))
    }
    if(studyPeriod[1] < "1800-01-01") {
      cli::cli_alert(paste0("The observation period in the cdm starts at a very early date."))
    }
  }

  if(is.na(studyPeriod[2])){
    studyPeriod[2] <- observationRange |>
      dplyr::pull("maxobs") |>
      as.character()
  } else {
    if(observationRange |>
       dplyr::pull("maxobs") < studyPeriod[2]) {
      cli::cli_alert(paste0("The observation period in the cdm ends in ",observationRange |>
                              dplyr::pull("maxobs")))
    }
    if(studyPeriod[2] > clock::date_today(zone = "GMT")) {
      cli::cli_alert(paste0("The observation period in the cdm ends after current date."))
    }
  }

  return(studyPeriod |> as.Date())
}

#' @noRd
validateFacet <- function(facet, result, call = parent.frame()) {

  if(rlang::is_formula(facet)){
    facet <- as.character(facet)
    facet <- unlist(strsplit(facet, " \\+ "))
    facet <- facet[facet != "~" & facet != "+" & facet != "."]
  }
    # Assert choices
    optFacetColour <- visOmopResults::tidyColumns(result)
    optFacetColour <- optFacetColour[optFacetColour %in% visOmopResults::tidyColumns(result)]

    facet <- as.character(facet)
    omopgenerics::assertChoice(facet, optFacetColour, null = TRUE, call = call)

  return(invisible(NULL))
}
