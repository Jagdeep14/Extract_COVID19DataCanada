test_that("testing of summary_of_canada", {
  expected <- summary_of_canada()
  expect_s3_class(expected, "data.frame")
})
