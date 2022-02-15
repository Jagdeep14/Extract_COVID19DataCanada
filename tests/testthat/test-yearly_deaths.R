test_that("testing of yearly_deaths", {
  expected <- yearly_deaths("Alberta")
  expect_s3_class(expected, "data.frame")
  expect_equal(unique(expected$province), "Alberta")
  expect_error(yearly_deaths("BC"), "Please enter a valid province name that too in its full form!")
})
