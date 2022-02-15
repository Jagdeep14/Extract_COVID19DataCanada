test_that("testing of yearly_vaccine_distribution", {
  expected <- yearly_vaccine_distribution("Alberta")
  expect_s3_class(expected, "data.frame")
  expect_equal(unique(expected$province), "Alberta")
  expect_error(yearly_vaccine_distribution("BC"), "Please enter a valid province name that too in its full form!")
})
