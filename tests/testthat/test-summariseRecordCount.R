test_that("summariseRecordCount() works", {

  # Load mock database ----
  cdm <- cdmEunomia()

  # Check inputs ----
  expect_true(inherits(summariseRecordCount(omopTable = cdm$observation_period, unit = "month"),"summarised_result"))
  expect_true(inherits(summariseRecordCount(omopTable = cdm$observation_period, unitInterval = 5),"summarised_result"))

  expect_no_error(summariseRecordCount(cdm$observation_period))
  expect_no_error(summariseRecordCount(cdm$visit_occurrence))
  expect_no_error(summariseRecordCount(cdm$condition_occurrence))
  expect_no_error(summariseRecordCount(cdm$drug_exposure))
  expect_no_error(summariseRecordCount(cdm$procedure_occurrence))
  expect_warning(summariseRecordCount(cdm$device_exposure))
  expect_no_error(summariseRecordCount(cdm$measurement))
  expect_no_error(summariseRecordCount(cdm$observation))
  expect_warning(summariseRecordCount(cdm$death))

  # Check inputs ----
  expect_true(
    (summariseRecordCount(cdm$observation_period) |>
       dplyr::filter(variable_level == "1963-01-01 to 1963-12-31") |>
       dplyr::pull("estimate_value") |>
       as.numeric()) ==
      (cdm$observation_period |>
         dplyr::ungroup() |>
         dplyr::mutate(year = clock::get_year(observation_period_start_date)) |>
         dplyr::filter(year == 1963) |>
         dplyr::tally() |>
         dplyr::pull("n"))
  )

  expect_true(
  summariseRecordCount(cdm$condition_occurrence, unit = "month") |>
    dplyr::filter(variable_level == "1961-02-01 to 1961-02-28") |>
    dplyr::pull("estimate_value") |>
    as.numeric() ==
  (cdm$condition_occurrence |>
      dplyr::ungroup() |>
      dplyr::mutate(year = clock::get_year(condition_start_date)) |>
      dplyr::mutate(month = clock::get_month(condition_start_date)) |>
      dplyr::filter(year == 1961, month == 2) |>
      dplyr::tally() |>
      dplyr::pull("n"))
  )

  expect_true(
    (summariseRecordCount(cdm$condition_occurrence, unit = "month", unitInterval = 3) |>
      dplyr::filter(variable_level %in% c("1984-01-01 to 1984-03-31")) |>
      dplyr::pull("estimate_value") |>
      as.numeric()) ==
      (cdm$condition_occurrence |>
         dplyr::ungroup() |>
         dplyr::mutate(year = clock::get_year(condition_start_date)) |>
         dplyr::mutate(month = clock::get_month(condition_start_date)) |>
         dplyr::filter(year == 1984, month %in% c(1:3)) |>
         dplyr::tally() |>
         dplyr::pull("n"))
  )

  expect_true(
    (summariseRecordCount(cdm$drug_exposure, unitInterval = 8) |>
       dplyr::filter(variable_level == "1981-01-01 to 1988-12-31") |>
       dplyr::pull("estimate_value") |>
       as.numeric()) ==
      (cdm$drug_exposure |>
         dplyr::ungroup() |>
         dplyr::mutate(year = clock::get_year(drug_exposure_start_date)) |>
         dplyr::filter(year %in% c(1981:1988)) |>
         dplyr::tally() |>
         dplyr::pull("n"))
  )

  PatientProfiles::mockDisconnect(cdm = cdm)
})

test_that("plotRecordCount() works", {
  # Load mock database ----
  cdm <- cdmEunomia()

  p <- summariseRecordCount(cdm$drug_exposure, unitInterval = 8) |>
    plotRecordCount()

  expect_true(inherits(p,"ggplot"))

  # expect_warning(inherits(plotRecordCount(summariseRecordCount(cdm$death, unitInterval = 8)),"ggplot"))

  PatientProfiles::mockDisconnect(cdm = cdm)
})

test_that("summariseRecordCount() ageGroup argument works", {
  # Load mock database ----
  cdm <- cdmEunomia()

  # Check that works ----
  expect_no_error(t <- summariseRecordCount(cdm$condition_occurrence, ageGroup = list(">=65" = c(65, Inf), "<65" = c(0,64))))
  x <- t |>
    dplyr::select("strata_level", "variable_level", "estimate_value") |>
    dplyr::filter(strata_level != "overall") |>
    dplyr::group_by(variable_level) |>
    dplyr::summarise(estimate_value = sum(as.numeric(estimate_value))) |>
    dplyr::arrange(variable_level) |>
    dplyr::pull("estimate_value")
  y <- t |>
    dplyr::select("strata_level", "variable_level", "estimate_value") |>
    dplyr::filter(strata_level == "overall") |>
    dplyr::arrange(variable_level) |>
    dplyr::mutate(estimate_value = as.numeric(estimate_value)) |>
    dplyr::pull("estimate_value")
  expect_equal(x,y)

  expect_no_error(t <- summariseRecordCount(cdm$condition_occurrence, ageGroup = list("<=20" = c(0,20), "21 to 40" = c(21,40), "41 to 60" = c(41,60), ">60" = c(61, Inf))))
  x <- t |>
    dplyr::select("strata_level", "variable_level", "estimate_value") |>
    dplyr::filter(strata_level != "overall") |>
    dplyr::group_by(variable_level) |>
    dplyr::summarise(estimate_value = sum(as.numeric(estimate_value))) |>
    dplyr::arrange(variable_level) |>
    dplyr::pull("estimate_value")
  y <- t |>
    dplyr::select("strata_level", "variable_level", "estimate_value") |>
    dplyr::filter(strata_level == "overall") |>
    dplyr::arrange(variable_level) |>
    dplyr::mutate(estimate_value = as.numeric(estimate_value)) |>
    dplyr::pull("estimate_value")
  expect_equal(x,y)

  expect_no_error(t <- summariseRecordCount(cdm$condition_occurrence, ageGroup = list("<=20" = c(0,20), "21 to 40" = c(21,40), "41 to 60" = c(41,60), ">60" = c(61, Inf))))
   x <- t |>
    dplyr::select("strata_level", "variable_level", "estimate_value") |>
    dplyr::filter(strata_level == "<=20" & variable_level == "1920-01-01 to 1920-12-31") |>
    dplyr::summarise(n = sum(as.numeric(estimate_value))) |>
    dplyr::pull("n")
  y <- cdm$condition_occurrence |>
    PatientProfiles::addAgeQuery(indexDate = "condition_start_date", ageGroup = list("<=20" = c(0,20))) |>
    dplyr::filter(age_group == "<=20") |>
    dplyr::filter(clock::get_year(condition_start_date) == "1920") |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::pull("n") |> as.numeric()
  expect_equal(x,y)

  PatientProfiles::mockDisconnect(cdm = cdm)
})


test_that("summariseRecordCount() sex argument works", {
  # Load mock database ----
  cdm <- cdmEunomia()

  # Check that works ----
  expect_no_error(t <- summariseRecordCount(cdm$condition_occurrence, sex = TRUE))
  x <- t |>
    dplyr::select("strata_level", "variable_level", "estimate_value") |>
    dplyr::filter(strata_level != "overall") |>
    dplyr::group_by(variable_level) |>
    dplyr::summarise(estimate_value = sum(as.numeric(estimate_value))) |>
    dplyr::arrange(variable_level) |>
    dplyr::pull("estimate_value")
  y <- t |>
    dplyr::select("strata_level", "variable_level", "estimate_value") |>
    dplyr::filter(strata_level == "overall") |>
    dplyr::arrange(variable_level) |>
    dplyr::mutate(estimate_value = as.numeric(estimate_value)) |>
    dplyr::pull("estimate_value")
  expect_equal(x,y)

  expect_no_error(t <- summariseRecordCount(cdm$observation_period, sex = TRUE))
  x <- t |>
    dplyr::select("strata_level", "variable_level", "estimate_value") |>
    dplyr::filter(strata_level != "overall") |>
    dplyr::group_by(variable_level) |>
    dplyr::summarise(estimate_value = sum(as.numeric(estimate_value))) |>
    dplyr::arrange(variable_level) |>
    dplyr::pull("estimate_value")
  y <- t |>
    dplyr::select("strata_level", "variable_level", "estimate_value") |>
    dplyr::filter(strata_level == "overall") |>
    dplyr::arrange(variable_level) |>
    dplyr::mutate(estimate_value = as.numeric(estimate_value)) |>
    dplyr::pull("estimate_value")
  expect_equal(x,y)

  expect_no_error(t <- summariseRecordCount(cdm$condition_occurrence, sex = TRUE))
  x <- t |>
    dplyr::select("strata_level", "variable_level", "estimate_value") |>
    dplyr::filter(strata_level == "Male", variable_level == "1937-01-01 to 1937-12-31") |> dplyr::pull(estimate_value)

  y <- cdm$condition_occurrence |>
    PatientProfiles::addSexQuery() |>
    dplyr::filter(sex == "Male") |>
    dplyr::mutate(year = clock::get_year(condition_start_date)) |>
    dplyr::filter(year == 1937) |>
    dplyr::summarise(n = n()) |>
    dplyr::pull(n) |>
    as.character()
  expect_equal(x,y)

  PatientProfiles::mockDisconnect(cdm = cdm)
})




