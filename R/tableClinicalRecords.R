
#' Create a visual table from a summariseClinicalRecord() output.
#'
#' @param result Output from summariseClinicalRecords().
#' @param type Type of formatting output table, either "gt" or "flextable".
#' @return A gt or flextable object with the summarised data.
#' @export
#' @examples
#' \donttest{
#' cdm <- mockOmopSketch()
#'
#' summarisedResult <- summariseClinicalRecords(
#'   cdm = cdm,
#'   omopTableName = c("condition_occurrence", "drug_exposure"),
#'   recordsPerPerson = c("mean", "sd"),
#'   inObservation = TRUE,
#'   standardConcept = TRUE,
#'   sourceVocabulary = TRUE,
#'   domainId = TRUE,
#'   typeConcept = TRUE
#' )
#'
#' summarisedResult |>
#'   suppress(minCellCount = 5) |>
#'   tableClinicalRecords()
#'
#' PatientProfiles::mockDisconnect(cdm)
#'}
tableClinicalRecords <- function(result,
                                 type = "gt") {
  # initial checks
  omopgenerics::validateResultArgument(result)
  omopgenerics::assertChoice(type, choicesTables())

  # subset to result_type of interest
  result <- result |>
    visOmopResults::filterSettings(
      .data$result_type == "summarise_clinical_records")

  # check if it is empty
  if (nrow(result) == 0) {
    warnEmpty("summarise_clinical_records")
    return(emptyTable(type))
  }

  result |>
    formatColumn(c("variable_name", "variable_level")) |>
    visOmopResults::visOmopTable(
      type = type,
      estimateName = c(
        "N (%)" = "<count> (<percentage>%)",
        "N" = "<count>",
        "Mean (SD)" = "<mean> (<sd>)"),
      header = c("cdm_name"),
      rename = c("Database name" = "cdm_name"),
      groupColumn = c("omop_table", visOmopResults::strataColumns(result))
    )
}
