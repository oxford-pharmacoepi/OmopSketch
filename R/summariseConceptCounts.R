
#' Summarise code use in patient-level data
#'
#' @param cdm A cdm object
#' @param conceptId List of concept IDs to summarise.
#' @param countBy Either "record" for record-level counts or "person" for
#' person-level counts
#' @param concept TRUE or FALSE. If TRUE code use will be summarised by concept.
#' @param year TRUE or FALSE. If TRUE code use will be summarised by year.
#' @param sex TRUE or FALSE. If TRUE code use will be summarised by sex.
#' @param ageGroup A list of ageGroup vectors of length two. Code use will be
#' thus summarised by age groups.
#' @return A summarised_result object with results overall and, if specified, by
#' strata.
#' @export
#' @examples
#' \donttest{
#'
#' cdm <- mockOmopSketch()
#'
#' cs <- list(sumatriptan = c(35604883, 35604879, 35604880, 35604884))
#'
#' results <- summariseConceptCounts(cdm, conceptId = cs)
#'
#' results
#'
#' PatientProfiles::mockDisconnect(cdm)
#' }
summariseConceptCounts <- function(cdm,
                                   conceptId,
                                   countBy = c("record", "person"),
                                   concept = TRUE,
                                   year = FALSE,
                                   sex = FALSE,
                                   ageGroup = NULL){

  omopgenerics::validateCdmArgument(cdm)
  omopgenerics::assertList(conceptId, named = TRUE)
  checkCountBy(countBy)
  omopgenerics::assertChoice(countBy, choices = c("record", "person"))
  countBy <- gsub("persons","subjects",paste0("number ",countBy,"s"))

  # Get all concepts in concept table if conceptId is NULL
  # if(is.null(conceptId)) {
  #   conceptId <- cdm$concept |>
  #     dplyr::select("concept_name", "concept_id") |>
  #     dplyr::collect() |>
  #     dplyr::group_by(.data$concept_name)  |>
  #     dplyr::summarise(named_vec = list(.data$concept_id)) |>
  #     tibble::deframe()
  # }

  getAllCodeUse <- function() {
    codeUse <- list()
    cli::cli_progress_bar("Getting use of codes", total = length(conceptId))
    for(i in 1:length(conceptId)) {
      cli::cli_alert_info("Getting concept counts of {names(conceptId)[i]}")
      codeUse[[i]] <- getCodeUse(conceptId[i],
                                 cdm = cdm,
                                 cohortTable = NULL,
                                 cohortId = NULL,
                                 timing = "any",
                                 countBy = countBy,
                                 concept = concept,
                                 year = year,
                                 sex = sex,
                                 ageGroup = ageGroup)
      Sys.sleep(i/length(conceptId))
      cli::cli_progress_update()
    }
    codeUse <- codeUse |>
      dplyr::bind_rows()
    cli::cli_progress_done()
    return(codeUse)
  }
  codeUse <- getAllCodeUse()

  if(nrow(codeUse) > 0) {
    codeUse <- codeUse %>%
      dplyr::mutate(
        result_id = as.integer(1),
        cdm_name = omopgenerics::cdmName(cdm)
      ) %>%
      omopgenerics::newSummarisedResult(
        settings = dplyr::tibble(
          result_id = as.integer(1),
          result_type = "summarise_concept_counts",
          package_name = "OmopSketch",
          package_version = as.character(utils::packageVersion("OmopSketch"))
        )
      )
  } else {
    codeUse <- omopgenerics::emptySummarisedResult()
  }

  return(codeUse)
}

getCodeUse <- function(x,
                       cdm,
                       cohortTable,
                       cohortId,
                       timing,
                       countBy,
                       concept,
                       year,
                       sex,
                       ageGroup,
                       call = parent.frame()) {

  tablePrefix <-  omopgenerics::tmpPrefix()

  omopgenerics::assertCharacter(timing, len = 1)
  omopgenerics::assertChoice(timing, choices = c("any", "entry"))
  omopgenerics::assertNumeric(x[[1]], integerish = TRUE)
  omopgenerics::assertList(x)
  omopgenerics::assertLogical(concept)
  omopgenerics::assertLogical(year)
  omopgenerics::assertLogical(sex)
  ageGroup <- omopgenerics::validateAgeGroupArgument(ageGroup, ageGroupName = "")[[1]]

  tableCodelist <- paste0(tablePrefix,"codelist")
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = tableCodelist,
                                   table = dplyr::tibble(concept_id = x[[1]]),
                                   overwrite = TRUE,
                                   temporary = FALSE)

  cdm[[tableCodelist]] <- cdm[[tableCodelist]] %>%
    dplyr::left_join(
      cdm[["concept"]] %>% dplyr::select("concept_id", "domain_id"),
      by = "concept_id"
    )

  tableDomainsData <- paste0(tablePrefix,"domains_data")
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = tableDomainsData,
                                   table = tables,
                                   overwrite = TRUE,
                                   temporary = FALSE)

  cdm[[tableCodelist]] <- cdm[[tableCodelist]] %>%
    dplyr::mutate(domain_id = tolower(.data$domain_id)) |>
    dplyr::left_join(cdm[[tableDomainsData]],
                     by = "domain_id") |>
    dplyr::compute(name = tableCodelist,
                   temporary = FALSE,
                   overwrite = TRUE)

  intermediateTable <- paste0(tablePrefix,"intermediate_table")
  records <- getRelevantRecords(cdm = cdm,
                                tableCodelist = tableCodelist,
                                cohortTable = cohortTable,
                                cohortId = cohortId,
                                timing = timing,
                                intermediateTable = intermediateTable)

  if(is.null(records)){
    cc <- dplyr::tibble()
    cli::cli_inform(c(
      "i" = "No records found in the cdm for the concepts provided."
    ))
    return(omopgenerics::emptySummarisedResult())
  }

  records <- addStrataToOmopTable(records, "date", ageGroup, sex)

   # if(!is.null(records) &&
   #   (records %>% utils::head(1) %>% dplyr::tally() %>% dplyr::pull("n") > 0)) {
   #  if(sex == TRUE | !is.null(ageGroup)){
   #    records <- records %>%
   #      PatientProfiles::addDemographicsQuery(age = !is.null(ageGroup),
   #                                            ageGroup = ageGroup,
   #                                            sex = sex,
   #                                            priorObservation = FALSE,
   #                                            futureObservation =  FALSE,
   #                                            indexDate = "date") |>
   #      dplyr::compute(overwrite = TRUE,
   #                     name = omopgenerics::tableName(records),
   #                     temporary = FALSE)
   #  }
  strata <- getStrataList(sex,ageGroup)
  if(year){strata <- omopgenerics::combineStrata(c("year", unique(unlist(strata))))}







  cc <- records |>
    dplyr::distinct() |>
    dplyr::collect() |> # https://github.com/darwin-eu-dev/PatientProfiles/issues/706
    PatientProfiles::summariseResult(strata = strata,
                                     variable = "standard_concept_name",
                                     group = "standard_concept_id",
                                     includeOverallGroup = TRUE,
                                     includeOverallStrata = TRUE,
                                     counts = TRUE,
                                     estimates = c("count")) |>
    suppressMessages() |>
    dplyr::filter(.data$variable_name %in% .env$countBy) |>
    dplyr::mutate(estimate_name  = dplyr::if_else(.data$variable_name == "number records", "record_count", "person_count"),
                  variable_level = dplyr::if_else(.data$group_level == "overall", NA, .data$group_level)) |>
    dplyr::mutate(group_name = "codelist_name") |>
    dplyr::mutate(group_level = names(x)) |>
    dplyr::mutate(cdm_name = omopgenerics::cdmName(cdm)) |>
    dplyr::select(-c("variable_name", "additional_name", "additional_level")) |>
    dplyr::left_join(
      getConceptsInfo(records),
      by = "variable_level"
    ) |>
    dplyr::mutate("additional_name"  = dplyr::if_else(is.na(.data$additional_name), "overall", .data$additional_name),
                  "additional_level" = dplyr::if_else(is.na(.data$additional_level), "overall", .data$additional_level))

    # byAgeGroup <- !is.null(ageGroup)
    # codeCounts <- getSummaryCounts(records = records1,
    #                                cdm = cdm,
    #                                countBy = countBy,
    #                                concept = concept,
    #                                year = year,
    #                                sex = sex,
    #                                byAgeGroup = byAgeGroup)
    #
    # if (is.null(cohortTable)) {
    #   cohortName <- NA
    # } else {
    #   cohortName <- omopgenerics::settings(cdm[[cohortTable]]) %>%
    #     dplyr::filter(.data$cohort_definition_id == cohortId) %>%
    #     dplyr::pull("cohort_name")
    # }
    #
    # codeCounts <-  codeCounts %>%
    #   dplyr::mutate(
    #     "codelist_name" := !!names(x),
    #     "cohort_name" = .env$cohortName,
    #     "estimate_type" = "integer",
    #     "variable_name" = dplyr::if_else(is.na(.data$standard_concept_name), "overall", .data$standard_concept_name),
    #     "variable_level" = as.character(.data$standard_concept_id)
    #   ) %>%
    #   visOmopResults::uniteGroup(cols = c("cohort_name", "codelist_name")) %>%
    #   visOmopResults::uniteAdditional(
    #     cols = c("source_concept_name", "source_concept_id", "domain_id")
    #   ) %>%
    #   dplyr::select(
    #     "group_name", "group_level", "strata_name", "strata_level",
    #     "variable_name", "variable_level", "estimate_name", "estimate_type",
    #     "estimate_value", "additional_name", "additional_level"
    #   )

  CDMConnector::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  return(cc)
}

getRelevantRecords <- function(cdm,
                               tableCodelist,
                               cohortTable,
                               cohortId,
                               timing,
                               intermediateTable){

  codes <- cdm[[tableCodelist]] |> dplyr::collect()

  tableName <- purrr::discard(unique(codes$table_name), is.na)
  standardConceptIdName <- purrr::discard(unique(codes$standard_concept), is.na)
  sourceConceptIdName <- purrr::discard(unique(codes$source_concept), is.na)
  dateName <- purrr::discard(unique(codes$start_date), is.na)

  if(!is.null(cohortTable)){
    if(is.null(cohortId)){
      cohortSubjects <- cdm[[cohortTable]] %>%
        dplyr::select("subject_id", "cohort_start_date") %>%
        dplyr::rename("person_id" = "subject_id") %>%
        dplyr::distinct()
    } else {
      cohortSubjects <- cdm[[cohortTable]] %>%
        dplyr::filter(.data$cohort_definition_id %in% cohortId) %>%
        dplyr::select("subject_id", "cohort_start_date") %>%
        dplyr::rename("person_id" = "subject_id") %>%
        dplyr::distinct()
    }
  }

  if(length(tableName)>0){
    codeRecords <- cdm[[tableName[[1]]]]
    if(!is.null(cohortTable)){
      # keep only records of those in the cohorts of interest
      codeRecords <- codeRecords %>%
        dplyr::inner_join(cohortSubjects,
                          by = "person_id")
      if(timing == "entry"){
        codeRecords <- codeRecords %>%
          dplyr::filter(.data$cohort_start_date == !!dplyr::sym(dateName[[1]]))
      }
    }

    if(is.null(codeRecords)){
      return(NULL)
    }

    tableCodes <- paste0(omopgenerics::uniqueTableName(),
                         omopgenerics::uniqueId())
    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = tableCodes,
                                     table = codes %>%
                                       dplyr::filter(.data$table_name == !!tableName[[1]]) %>%
                                       dplyr::select("concept_id", "domain_id"),
                                     overwrite = TRUE,
                                     temporary = FALSE)

    codeRecords <- codeRecords %>%
      dplyr::mutate(date = !!dplyr::sym(dateName[[1]])) %>%
      dplyr::mutate(year = clock::get_year(date)) %>%
      dplyr::select(dplyr::all_of(c("person_id",
                                    standardConceptIdName[[1]],
                                    sourceConceptIdName[[1]],
                                    "date", "year"))) %>%
      dplyr::rename("standard_concept_id" = .env$standardConceptIdName[[1]],
                    "source_concept_id" = .env$sourceConceptIdName[[1]]) %>%
      dplyr::inner_join(cdm[[tableCodes]],
                        by = c("standard_concept_id"="concept_id")) %>%
      dplyr::compute(
        name = paste0(intermediateTable,"_grr"),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )

    CDMConnector::dropTable(cdm = cdm, name = tableCodes)
    cdm[[tableCodes]] <- NULL

  } else {
    return(NULL)
  }

  # get for any additional domains and union
  if(length(tableName) > 1) {
    for(i in 1:(length(tableName)-1)) {
      workingRecords <-  cdm[[tableName[[i+1]]]]
      if(!is.null(cohortTable)){
        # keep only records of those in the cohorts of interest
        workingRecords <- workingRecords %>%
          dplyr::inner_join(cohortSubjects,
                            by = "person_id")
        if(timing == "entry"){
          workingRecords <- workingRecords %>%
            dplyr::filter(.data$cohort_start_date == !!dplyr::sym(dateName[[i+1]]))
        }
      }
      workingRecords <-  workingRecords %>%
        dplyr::mutate(date = !!dplyr::sym(dateName[[i+1]])) %>%
        dplyr::mutate(year = clock::get_year(date)) %>%
        dplyr::select(dplyr::all_of(c("person_id",
                                      standardConceptIdName[[i+1]],
                                      sourceConceptIdName[[i+1]],
                                      "date", "year"))) %>%
        dplyr::rename("standard_concept_id" = .env$standardConceptIdName[[i+1]],
                      "source_concept_id" = .env$sourceConceptIdName[[i+1]]) %>%
        dplyr::inner_join(codes %>%
                            dplyr::filter(.data$table_name == tableName[[i+1]]) %>%
                            dplyr::select("concept_id", "domain_id"),
                          by = c("standard_concept_id"="concept_id"),
                          copy = TRUE)

      if(workingRecords %>% utils::head(1) %>% dplyr::tally() %>% dplyr::pull("n") >0){
        codeRecords <- codeRecords %>%
          dplyr::union_all(workingRecords)  %>%
          dplyr::compute(
            name = paste0(intermediateTable,"_grr_i"),
            temporary = FALSE,
            schema = attr(cdm, "write_schema"),
            overwrite = TRUE
          )
      }
    }
  }

  if(codeRecords %>% utils::head(1) %>% dplyr::tally() %>% dplyr::pull("n") >0){
    codeRecords <- codeRecords %>%
      dplyr::left_join(cdm[["concept"]] %>%
                         dplyr::select("concept_id", "concept_name"),
                       by = c("standard_concept_id"="concept_id")) %>%
      dplyr::rename("standard_concept_name"="concept_name") %>%
      dplyr::left_join(cdm[["concept"]] %>%
                         dplyr::select("concept_id", "concept_name"),
                       by = c("source_concept_id"="concept_id")) %>%
      dplyr::rename("source_concept_name"="concept_name")  %>%
      dplyr::mutate(source_concept_name = dplyr::if_else(is.na(.data$source_concept_name),
                                                         "NA", .data$source_concept_name)) %>%
      dplyr::compute(
        name = paste0(intermediateTable,"_grr_cr"),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )
  }

  return(codeRecords)
}

getConceptsInfo <- function(records){
  records |>
    dplyr::select("standard_concept_name", "standard_concept_id", "source_concept_name", "source_concept_id", "domain_id") |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::mutate("additional_name"  = "source_concept_name &&& source_concept_id &&& domain_id") |>
    dplyr::mutate("additional_level" = paste0(.data$source_concept_name, " &&& ", .data$source_concept_id, " &&& ", .data$domain_id)) |>
    dplyr::select("variable_name" = "standard_concept_name", "variable_level" = "standard_concept_id","additional_name", "additional_level") |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::add_row(
      "variable_name"  = "overall",
      "variable_level" = NA,
      "additional_name"  = "overall",
      "additional_level" = "overall"
    )
}

