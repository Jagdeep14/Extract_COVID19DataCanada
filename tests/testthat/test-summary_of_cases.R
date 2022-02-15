test_that("testing of summary_of_cases", {
  expected <- summary_of_cases("Ontario")
  expect_s3_class(expected, "data.frame")
  expect_equal(unique(expected$province), "Ontario")
  expect_error(summary_of_cases("BC"), "Please enter a valid province name in full form!")
})
