test_that("testing of summary_of_peaktime", {
  expected <- province_population("Ontario")
  expect_s3_class(expected, "data.frame")
  expect_equal(unique(expected$province), "Ontario")
  expect_error(province_population("BC"), "Please enter a valid province name in full form!")
})
