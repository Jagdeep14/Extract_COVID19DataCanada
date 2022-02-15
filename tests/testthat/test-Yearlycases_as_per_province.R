test_that("testing of Yearlycases_as_per_province", {
  expected <- Yearlycases_as_per_province("British Columbia","2021")
  expect_s3_class(expected, "data.frame")
  expect_equal(unique(expected$province), "British Columbia")
  expect_equal(unique(expected$year), "2021")
  expect_error(Yearlycases_as_per_province("BC"), "Please enter a valid province name that too in its full form!")
})
