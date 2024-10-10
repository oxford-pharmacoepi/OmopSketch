test_that("summarise code use - eunomia", {
  skip_on_cran()
  cdm <- cdmEunomia()
  acetiminophen <- c(1125315,  1127433, 40229134,
                     40231925, 40162522, 19133768,  1127078)
  poliovirus_vaccine <- c(40213160)
  cs <- list(acetiminophen = acetiminophen,
             poliovirus_vaccine = poliovirus_vaccine)
  startNames <- CDMConnector::listSourceTables(cdm)
  results <- summariseConceptCounts(cdm = cdm,
                                    conceptId = cs,
                                    year = TRUE,
                                    sex = TRUE,
                                    ageGroup = list(c(0,17),
                                                    c(18,65),
                                                    c(66, 100)))
  endNames <- CDMConnector::listSourceTables(cdm)
  expect_true(length(setdiff(endNames, startNames)) == 0)

  #Check result type
  checkResultType(results, "summarise_concept_counts")

  # min cell counts:
  expect_equal(
      omopgenerics::suppress(results) |>
        dplyr::filter(
          variable_name == "overall",
          strata_level == "1909",
          group_level == "acetiminophen"
        ) |>
        dplyr::pull("estimate_value"),
      as.character(c(NA,NA))
  )

  # check is a summarised result
  expect_true("summarised_result" %in%  class(results))
  expect_equal(omopgenerics::resultColumns(),
               colnames(results))

  # overall record count
  expect_true(results %>%
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "overall" &
                                strata_level == "overall" &
                                group_level == "acetiminophen" &
                                estimate_name == "record_count",
                              variable_name == "overall") %>%
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$drug_exposure %>%
                dplyr::filter(drug_concept_id %in%  acetiminophen) %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # overall person count
  expect_true(results %>%
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "overall" &
                                strata_level == "overall" &
                                group_level == "acetiminophen" &
                                estimate_name == "person_count",
                              variable_name == "overall") %>%
                dplyr::pull("estimate_value") |>
                as.numeric()  ==
                cdm$drug_exposure %>%
                dplyr::filter(drug_concept_id %in% acetiminophen) %>%
                dplyr::select("person_id") %>%
                dplyr::distinct() %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # by year
  # overall record count
  expect_true(results %>%
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "year" &
                                strata_level == "2008" &
                                group_level == "acetiminophen" &
                                estimate_name == "record_count",
                              variable_name == "overall") %>%
                dplyr::pull("estimate_value") |>
                as.numeric()  ==
                cdm$drug_exposure %>%
                dplyr::filter(drug_concept_id %in% acetiminophen) %>%
                dplyr::filter(year(drug_exposure_start_date) == 2008) %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # overall person count
  expect_true(results %>%
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "year" &
                                strata_level == "2008" &
                                group_level == "acetiminophen" &
                                estimate_name == "person_count",
                              variable_name == "overall") %>%
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$drug_exposure %>%
                dplyr::filter(drug_concept_id %in% acetiminophen) %>%
                dplyr::filter(year(drug_exposure_start_date) == 2008) %>%
                dplyr::select("person_id") %>%
                dplyr::distinct() %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # by age group and sex
  # overall record count
  expect_true(results %>%
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "sex" &
                                strata_level == "Male" &
                                group_level == "acetiminophen" &
                                estimate_name == "record_count",
                              variable_name == "overall") %>%
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$drug_exposure %>%
                dplyr::filter(drug_concept_id %in% acetiminophen) %>%
                PatientProfiles::addSex() %>%
                dplyr::filter(sex == "Male") %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  expect_true(results %>%
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "age_group &&& sex" &
                                strata_level == "18 to 65 &&& Male" &
                                group_level == "acetiminophen" &
                                estimate_name == "record_count",
                              variable_name == "overall") %>%
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$drug_exposure %>%
                dplyr::filter(drug_concept_id %in% acetiminophen) %>%
                PatientProfiles::addAge(indexDate = "drug_exposure_start_date") %>%
                PatientProfiles::addSex() %>%
                dplyr::filter(sex == "Male" &
                                age >= "18" &
                                age <= "65") %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # overall person count
  expect_true(results %>%
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "age_group &&& sex" &
                                strata_level == "18 to 65 &&& Male" &
                                group_level == "acetiminophen" &
                                estimate_name == "person_count",
                              variable_name == "overall") %>%
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$drug_exposure %>%
                dplyr::filter(drug_concept_id %in% acetiminophen) %>%
                PatientProfiles::addAge(indexDate = "drug_exposure_start_date") %>%
                PatientProfiles::addSex() %>%
                dplyr::filter(sex == "Male" &
                                age >= "18" &
                                age <= "65") %>%
                dplyr::select("person_id") %>%
                dplyr::distinct() %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  results <- summariseConceptCounts(list("acetiminophen" = acetiminophen),
                              cdm = cdm, countBy = "person",
                              year = FALSE,
                              sex = FALSE,
                              ageGroup = NULL)
  expect_true(nrow(results %>%
                     dplyr::filter(estimate_name == "person_count")) > 0)
  expect_true(nrow(results %>%
                     dplyr::filter(estimate_name == "record_count")) == 0)

  results <- summariseConceptCounts(list("acetiminophen" = acetiminophen),
                              cdm = cdm, countBy = "record",
                              year = FALSE,
                              sex = FALSE,
                              ageGroup = NULL)
  expect_true(nrow(results %>%
                     dplyr::filter(estimate_name == "person_count")) == 0)
  expect_true(nrow(results %>%
                     dplyr::filter(estimate_name == "record_count")) > 0)

  # domains covered
  # condition
  expect_true(nrow(summariseConceptCounts(list(cs= c(4112343)),
                                    cdm = cdm,
                                    year = FALSE,
                                    sex = FALSE,
                                    ageGroup = NULL))>1)

  # visit
  expect_true(nrow(summariseConceptCounts(list(cs= c(9201)),
                                    cdm = cdm,
                                    year = FALSE,
                                    sex = FALSE,
                                    ageGroup = NULL))>1)

  # drug
  expect_true(nrow(summariseConceptCounts(list(cs= c(40213160)),
                                    cdm = cdm,
                                    year = FALSE,
                                    sex = FALSE,
                                    ageGroup = NULL))>1)

  # measurement
  expect_true(nrow(summariseConceptCounts(list(cs= c(3006322)),
                                    cdm = cdm,
                                    year = FALSE,
                                    sex = FALSE,
                                    ageGroup = NULL))>1)

  # procedure and condition
  expect_true(nrow(summariseConceptCounts(list(cs= c(4107731,4112343)),
                                    cdm = cdm,
                                    year = FALSE,
                                    sex = FALSE,
                                    ageGroup = NULL))>1)

  # no records
  expect_message(results <- summariseConceptCounts(list(cs= c(999999)),
                                             cdm = cdm,
                                             year = FALSE,
                                             sex = FALSE,
                                             ageGroup = NULL))
  expect_true(nrow(results) == 0)

  # conceptId NULL (but reduce the computational time by filtering concepts first)
  # cdm$concept <- cdm$concept |>
  #   dplyr::filter(grepl("k", concept_name))
  #
  # skip("conceptId = NULL not supported yet")
  # results <- summariseConceptCounts(cdm = cdm,
  #                                   year = FALSE,
  #                                   sex = FALSE,
  #                                   ageGroup = NULL)
  #
  # results_concepts <- results |>
  #   dplyr::select(variable_name) |>
  #   dplyr::distinct() |>
  #   dplyr::pull()
  # concepts <- cdm$concept |>
  #   dplyr::select(concept_name) |>
  #   dplyr::distinct() |>
  #   dplyr::pull()
  #
  # expect_true(all(results_concepts %in% c("overall",concepts)))

  # check attributes
  results <- summariseConceptCounts(cdm = cdm,
                                    conceptId = cs,
                                    year = TRUE,
                                    sex = TRUE,
                                    ageGroup = list(c(0,17),
                                                    c(18,65),
                                                    c(66, 100)))

  expect_true(omopgenerics::settings(results)$package_name == "OmopSketch")
  expect_true(omopgenerics::settings(results)$result_type == "summarise_concept_counts")
  expect_true(omopgenerics::settings(results)$package_version == packageVersion("OmopSketch"))

  # expected errors# expected errors
  expect_error(summariseConceptCounts("not a concept",
                                cdm = cdm,
                                year = FALSE,
                                sex = FALSE,
                                ageGroup = NULL))
  expect_error(summariseConceptCounts("123",
                                cdm = cdm,
                                year = FALSE,
                                sex = FALSE,
                                ageGroup = NULL))
  expect_error(summariseConceptCounts(list("123"), # not named
                                cdm = cdm,
                                year = FALSE,
                                sex = FALSE,
                                ageGroup = NULL))
  expect_error(summariseConceptCounts(list(a = 123),
                                cdm = "not a cdm",
                                year = FALSE,
                                sex = FALSE,
                                ageGroup = NULL))
  expect_error(summariseConceptCounts(list(a = 123),
                                cdm = cdm,
                                year = "Maybe",
                                sex = FALSE,
                                ageGroup = NULL))
  expect_error(summariseConceptCounts(list(a = 123),
                                cdm = cdm,
                                year = FALSE,
                                sex = "Maybe",
                                ageGroup = NULL))
  expect_error(summariseConceptCounts(list(a = 123),
                                cdm = cdm,
                                year = FALSE,
                                sex = FALSE,
                                ageGroup = list(c(18,17))))
  expect_error(summariseConceptCounts(list(a = 123),
                                cdm = cdm,
                                year = FALSE,
                                sex = FALSE,
                                ageGroup = list(c(0,17),
                                                c(15,20))))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("summarise code use - mock data", {
  person <- tibble::tibble(
    person_id = c(1L,2L),
    gender_concept_id = c(8532L,8507L),
    year_of_birth = c(1997L,1963L),
    month_of_birth = c(8L,1L),
    day_of_birth = c(22L,27L),
    race_concept_id = c(1L,1L),
    ethnicity_concept_id = c(1L,1L)
  )
  observation_period <- tibble::tibble(
    person_id = c(1L,2L),
    observation_period_id = c(1L,2L),
    observation_period_start_date = c(as.Date("2000-06-03"), as.Date("1999-05-04")),
    observation_period_end_date = c(as.Date("2013-08-03"), as.Date("2004-01-04")),
    period_type_concept_id = c(1L,1L)
  )
  condition_occurrence <- tibble::tibble(
    person_id = c(1L,1L,1L,2L,2L,2L,2L,2L),
    condition_concept_id = c(1L,3L,5L,1L,5L,5L,17L,17L),
    condition_start_date = c(as.Date("2002-06-30"), as.Date("2004-05-29"), as.Date("2001-12-20"),
                             as.Date("2000-03-10"), as.Date("2000-02-25"), as.Date("1999-07-15"),
                             as.Date("1999-06-06"), as.Date("2000-07-17")),
    condition_end_date = c(as.Date("2004-09-30"), as.Date("2009-05-29"), as.Date("2008-12-20"),
                             as.Date("2001-03-10"), as.Date("2001-12-25"), as.Date("2001-07-15"),
                             as.Date("2002-06-06"), as.Date("2000-11-17")),
    condition_occurrence_id = c(1L,2L,3L,4L,5L,6L,7L,8L),
    condition_type_concept_id = c(1L),
    condition_source_concept_id = c(as.integer(NA))
  )
  concept <- tibble::tibble(
    concept_id = c(1L,3L,5L,17L),
    concept_name = c("Musculoskeletal disorder", "Arthritis", "Osteoarthritis of hip", "Arthritis"),
    domain_id = c("Condition"),
    standard_concept = c("S","S","S",NA),
    vocabulary_id = c("SNOMED", "SNOMED", "SNOMED", "ICD10"),
    concept_class_id = c("Clinical Finding", "Clinical Finding", "Clinical Finding", "ICD Code"),
    concept_code = c("1234"),
    valid_start_date = c(as.Date(NA)),
    valid_end_date = c(as.Date(NA))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      condition_occurrence = condition_occurrence,
      concept = concept
    ),
    cdmName = "mock data"
  )
  cdm <- CDMConnector::copyCdmTo(
    con = connection(), cdm = cdm, schema = schema())

  conceptId <- list(
    "Arthritis" = c(17,3),
    "Musculoskeletal disorder" = c(1),
    "Osteoarthritis of hip" = c(5)
  )

  result <- summariseConceptCounts(cdm, conceptId)

  # Arthritis (codes 3 and 17), one record of 17 per ind and one record of 3 ind 1
  expect_equal(result |>
                 dplyr::filter(variable_name == "Arthritis") |>
                 dplyr::arrange(variable_level, estimate_name) |>
                 dplyr::pull(estimate_value),
               c("1", "2", "1", "1"))

  # Osteoarthritis (code 5), two records ind 2, one record ind 1
  expect_equal(result |>
                 dplyr::filter(variable_name == "Osteoarthritis of hip") |>
                 dplyr::arrange(variable_level, estimate_name) |>
                 dplyr::pull(estimate_value),
               c("2","3"))

  # Musculoskeletal disorder (code 1), one record each ind
  expect_equal(result |>
                 dplyr::filter(variable_name == "Musculoskeletal disorder") |>
                 dplyr::arrange(variable_level, estimate_name) |>
                 dplyr::pull(estimate_value),
               c("2","2"))

  result <- summariseConceptCounts(cdm, conceptId, ageGroup = list(c(0,2), c(3,150)), sex = TRUE)
  # Individuals belong to the same age group but to different sex groups

  # Arthritis (codes 3 and 17), one record of each per ind
  expect_equal(result |>
                 dplyr::filter(variable_name == "Arthritis" & strata_level == "Male") |>
                 dplyr::arrange(variable_level, estimate_name) |>
                 dplyr::pull(estimate_value),
               c("1","2"))
  expect_equal(result |>
                 dplyr::filter(variable_name == "Arthritis" & strata_level == "3 to 150 &&& Male") |>
                 dplyr::arrange(variable_level, estimate_name) |>
                 dplyr::pull(estimate_value),
               c("1","2"))
  expect_equal(result |>
                 dplyr::filter(variable_name == "Arthritis" & strata_level == "3 to 150") |>
                 dplyr::arrange(variable_level, estimate_name) |>
                 dplyr::pull(estimate_value),
               c("1","2","1","1"))

  # Osteoarthritis of hip (code 5), two records ind 2 and one ind 1
  expect_equal(result |>
                 dplyr::filter(variable_name == "Osteoarthritis of hip" & strata_level == "Female") |>
                 dplyr::tally() |>
                 dplyr::pull(),
               2)

  # Musculoskeletal disorder (code 1), one record each ind
  expect_equal(result |>
                 dplyr::filter(variable_name == "Musculoskeletal disorder" & strata_level == "3 to 150 &&& Female") |>
                 dplyr::arrange(variable_level, estimate_name) |>
                 dplyr::pull(estimate_value),
               c("1","1"))
  expect_equal(result |>
                 dplyr::filter(variable_name == "Musculoskeletal disorder" & strata_level == "3 to 150 &&& Male") |>
                 dplyr::arrange(variable_level, estimate_name) |>
                 dplyr::pull(estimate_value),
               c("1","1"))
  expect_equal(result |>
                 dplyr::filter(variable_name == "Musculoskeletal disorder" & strata_level == "3 to 150") |>
                 dplyr::arrange(variable_level, estimate_name) |>
                 dplyr::pull(estimate_value),
               c("2","2"))
  expect_equal(result |>
                 dplyr::filter(variable_name == "Musculoskeletal disorder" & strata_level == "overall") |>
                 dplyr::arrange(variable_level, estimate_name) |>
                 dplyr::pull(estimate_value),
               c("2","2"))

  PatientProfiles::mockDisconnect(cdm)
})

test_that("plot concept counts works", {
  skip_on_cran()
  # Load mock database ----
  cdm <- cdmEunomia()

  # summariseInObservationPlot plot ----
  x <- summariseConceptCounts(cdm, conceptId = list(codes = c(40213160)))
  expect_error(plotConceptCounts(x))
  x <- x |> dplyr::filter(estimate_name == "record_count")
  expect_no_error(plotConceptCounts(x))
  expect_true(inherits(plotConceptCounts(x), "ggplot"))

  x <- summariseConceptCounts(cdm,
                              conceptId = list("polio" = c(40213160),
                                               "acetaminophen" = c(1125315,  1127433, 40229134, 40231925, 40162522, 19133768,  1127078)))
  expect_error(plotConceptCounts(x))
  x <- x |> dplyr::filter(estimate_name == "record_count")
  expect_no_error(plotConceptCounts(x))
  expect_message(plotConceptCounts(x))
  expect_no_error(plotConceptCounts(x, facet = "codelist_name"))
  expect_no_error(plotConceptCounts(x, colour = "codelist_name"))

  x <-  x |> dplyr::filter(result_id == -1)
  expect_error(plotInObservation(x))


  PatientProfiles::mockDisconnect(cdm = cdm)
})
