test_that("testing of cumulativevaccine_as_per_province", {
  expected <- cumulativevaccine_as_per_province("Alberta")
  expect_s3_class(expected, "data.frame")
  expect_equal(unique(expected$province), "Alberta")
  expect_error(cumulativevaccine_as_per_province("BC"), "Please enter a valid province name that too in its full form!")
})
