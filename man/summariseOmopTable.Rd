% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summariseOmopTable.R
\name{summariseOmopTable}
\alias{summariseOmopTable}
\title{Summarise an omop_table from a cdm_reference object. You will obtain
information related to the number of records, number of subjects, whether the
records are in observation, number of present domains and number of present
concepts.}
\usage{
summariseOmopTable(
  omopTable,
  recordsPerPerson = c("mean", "sd", "median", "q25", "q75", "min", "max"),
  inObservation = TRUE,
  standardConcept = TRUE,
  sourceVocabulary = FALSE,
  domainId = TRUE,
  typeConcept = TRUE
)
}
\arguments{
\item{omopTable}{An omop_table object.}

\item{recordsPerPerson}{Estimates to summarise the number of records per
person.}

\item{inObservation}{Whether to include the percentage of records in
observation.}

\item{standardConcept}{Whether to summarise standard concept.}

\item{sourceVocabulary}{Whether to summarise source vocabulary.}

\item{domainId}{Whether to summarise domain id of standard concept id.}

\item{typeConcept}{Whether to summarise type concept id field.}
}
\value{
A summarised_result object with the summarised data.
}
\description{
Summarise an omop_table from a cdm_reference object. You will obtain
information related to the number of records, number of subjects, whether the
records are in observation, number of present domains and number of present
concepts.
}
