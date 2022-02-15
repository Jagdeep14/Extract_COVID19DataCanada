test_that("testing of yearly_recovered", {
  expected <- yearly_recovered("Alberta")
  expect_s3_class(expected, "data.frame")
  expect_equal(unique(expected$province), "Alberta")
  expect_error(yearly_recovered("BC"), "Please enter a valid province name that too in its full form!")
})
