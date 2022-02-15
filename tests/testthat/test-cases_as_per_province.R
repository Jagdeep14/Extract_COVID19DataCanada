test_that("testing of cases_as_per_province", {
  expected <- cases_as_per_province("British Columbia")
  expect_s3_class(expected, "data.frame")
  expect_equal(unique(expected$province), "British Columbia")
  expect_error(cases_as_per_province("BC"), "Please enter a valid province name that too in its full form!")
})
