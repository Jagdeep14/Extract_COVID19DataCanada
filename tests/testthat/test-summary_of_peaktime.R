test_that("testing of summary_of_peaktime", {
  expected <- summary_of_peaktime("Ontario")
  expect_s3_class(expected, "data.frame")
  expect_equal(unique(expected$province), "Ontario")
  expect_error(summary_of_peaktime("BC"), "Please enter a valid province name in full form!")
})
