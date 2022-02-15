test_that("testing of mortality", {
  expected <- mortality("Alberta")
  expect_s3_class(expected, "data.frame")
  expect_equal(unique(expected$province), "Alberta")
  expect_error(mortality("BC"), "Please enter a valid province name that too in its full form!")
})
