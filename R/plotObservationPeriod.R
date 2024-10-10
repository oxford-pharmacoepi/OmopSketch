
#' Create a plot from the output of summariseObservationPeriod().
#'
#' @param result A summarised_result object.
#' @param variableName The variable to plot it can be: "number subjects",
#' "records per person", "duration" or "days to next observation period".
#' @param plotType The plot type, it can be: "barplot", "boxplot" or
#' "densityplot".
#' @param facet Columns to colour by. See possible columns to colour by with:
#' `visOmopResults::tidyColumns()`.
#' @param colour Columns to colour by. See possible columns to colour by with:
#' `visOmopResults::tidyColumns()`.
#' @return A ggplot2 object.
#' @export
#' @examples
#' \donttest{
#' cdm <- mockOmopSketch(numberIndividuals = 100)
#'
#' result <- summariseObservationPeriod(cdm$observation_period)
#'
#' result |>
#'   plotObservationPeriod(
#'     variableName = "duration in days",
#'     plotType = "boxplot"
#'   )
#'
#' PatientProfiles::mockDisconnect(cdm)
#' }
plotObservationPeriod <- function(result,
                                  variableName = "number subjects",
                                  plotType = "barplot",
                                  facet = NULL,
                                  colour = NULL) {
  # initial checks
  omopgenerics::validateResultArgument(result)

  # subset to result_type of interest
  result <- result |>
    visOmopResults::filterSettings(
      .data$result_type == "summarise_observation_period")

  # check if it is empty
  if (nrow(result) == 0) {
    warnEmpty("summarise_observation_period")
    return(emptyPlot())
  }

  variableNames <- unique(availablePlotObservationPeriod()$variable_name)
  omopgenerics::assertChoice(variableName, variableNames, length = 1)
  plotTypes <- availablePlotObservationPeriod() |>
    dplyr::filter(.data$variable_name == .env$variableName) |>
    dplyr::pull("plot_type")
  omopgenerics::assertChoice(plotType, plotTypes, length = 1)

  result <- result |>
    dplyr::filter(.data$variable_name == .env$variableName)

  validateFacet(facet, result)

  optFacetColour <- visOmopResults::tidyColumns(result)
  optFacetColour <- optFacetColour[optFacetColour %in% visOmopResults::tidyColumns(result)]
  omopgenerics::assertChoice(facet, optFacetColour, null = TRUE, call = call)

  # this is due to bug in visOmopResults to remove in next release
  # https://github.com/darwin-eu/visOmopResults/issues/246
  if (length(facet) == 0) facet <- NULL
  if (length(colour) == 0) colour <- NULL

  if(length(unique(result$group_name)) == 1){
    result <- result |>
      dplyr::mutate(group_name  = "observation_period_ordinal",
             group_level = "Overall")
  }

  if (plotType == "barplot") {
    p <- visOmopResults::barPlot(
      result = result,
      x = "observation_period_ordinal",
      y = "count",
      facet = facet,
      colour = colour) +
      ggplot2::ylab(stringr::str_to_sentence(unique(result$variable_name)))
  } else if (plotType == "boxplot") {
    p <- visOmopResults::boxPlot(
      result = result,
      x = "observation_period_ordinal",
      facet = facet,
      colour = colour)
  } else {
    p <- visOmopResults::scatterPlot(
      result = result,
      x = "density_x",
      y = "density_y",
      line = TRUE,
      point = FALSE,
      ribbon = FALSE,
      facet = facet,
      colour = colour,
      group = optFacetColour) +
      ggplot2::xlab(stringr::str_to_sentence(unique(result$variable_name))) +
      ggplot2::ylab("Density")
  }

  return(p)
}

availablePlotObservationPeriod <- function() {
  dplyr::tribble(
    ~variable_name, ~plot_type, ~facet,
    "number subjects", "barplot", "cdm_name+observation_period_ordinal",
    "records per person", "densityplot", "cdm_name",
    "records per person", "boxplot", "cdm_name",
    "duration in days", "densityplot", "cdm_name+observation_period_ordinal",
    "duration in days", "boxplot", "cdm_name+observation_period_ordinal",
    "days to next observation period", "densityplot", "cdm_name+observation_period_ordinal",
    "days to next observation period", "boxplot", "cdm_name+observation_period_ordinal",
  )
}
needEstimates <- function(plotType) {
  dplyr::tribble(
    ~plot_type, ~estimate_name,
    "barplot", "count",
    "densityplot", "density_x",
    "densityplot", "density_y",
    "boxplot", "median",
    "boxplot", "q25",
    "boxplot", "q75",
    "boxplot", "min",
    "boxplot", "max"
  ) |>
    dplyr::filter(.data$plot_type == .env$plotType) |>
    dplyr::pull("estimate_name")
}
uniteVariable <- function(res, cols, colname, def) {
  if (length(cols) > 0) {
    res <- res |>
      tidyr::unite(
        col = !!colname, dplyr::all_of(cols), sep = " - ", remove = FALSE)
  } else {
    res <- res |> dplyr::mutate(!!colname := !!def)
  }
  return(res)
}
