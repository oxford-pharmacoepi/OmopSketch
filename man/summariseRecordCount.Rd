% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summariseRecordCount.R
\name{summariseRecordCount}
\alias{summariseRecordCount}
\title{Create a summarise result object to summarise record counts of an omop_table using a specific time interval. Only records that fall within the observation period are counted.}
\usage{
summariseRecordCount(
  omopTable,
  unit = "year",
  unitInterval = 1,
  ageGroup = NULL,
  sex = FALSE
)
}
\arguments{
\item{omopTable}{An omop table from a cdm object.}

\item{unit}{Whether to stratify by "year" or by "month".}

\item{unitInterval}{An integer. Number of years or months to include within the same interval.}

\item{ageGroup}{A list of age groups to stratify results by.}

\item{sex}{Boolean variable. Whether to stratify by sex (TRUE) or not (FALSE).}
}
\value{
A summarised_result object..
}
\description{
Create a summarise result object to summarise record counts of an omop_table using a specific time interval. Only records that fall within the observation period are counted.
}
\examples{
\donttest{
library(dplyr)
library(CDMConnector)
library(DBI)
library(duckdb)
library(OmopSketch)

# Connect to Eunomia database
if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))
if (!eunomia_is_available()) downloadEunomiaData()
con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomia_dir())
cdm <- CDMConnector::cdmFromCon(
con = con, cdmSchema = "main", writeSchema = "main"
)

# Run summarise clinical tables
summarisedResult <- summariseRecordCount(omopTable = cdm$condition_occurrence,
                                      unit = "year",
                                      unitInterval = 10,
                                      ageGroup = list("<=20" = c(0,20), ">20" = c(21, Inf)),
                                      sex = TRUE)
summarisedResult |> print()
}
}
