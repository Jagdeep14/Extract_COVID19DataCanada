test_that("testing of active_cases", {
  expected <- active_cases("Alberta")
  expect_s3_class(expected, "data.frame")
  expect_equal(unique(expected$province), "Alberta")
  expect_error(active_cases("BC"), "Please enter a valid province name in full form!")
})
