library(testthat)
test_that("create_numeric_input input",{
  #checking that min_user is not negative and that it does not work when it is
  expect_error(create_numeric_input(min_user = -1))
  #checking that max_user is not character and that it does not work when it is
  expect_error(create_numeric_input(max_user = "text"))
})
#A MODIFIER:
test_that("estimate_area output",{
  #checking that output of estimated area is a double
  expect_type(estimate_area(B = 1000)$estimated_area, "double")
  #checking that output of points is a list
  expect_type(estimate_area(B = 1000)$points, "list")
  #checking that general output of the function is a list
  expect_type(estimate_area(B = 1000), "list")

})
