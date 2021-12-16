library(testthat)
test_that("newcommer_recom input",{
  #checking that age is not character and that it does not work when it is
  expect_error(newcommer_recom(anime, age = "text", gender = "action", freetime = 60))
  #checking that free time is not negative and that it does not work when it is
  expect_error(newcommer_recom(freetime = -1))
  #checking that free time is not character and that it does not work when it is
  expect_error(newcommer_recom(freetime = "text"))
})
#A MODIFIER:
test_that("newcommer_recom output",{
  #checking that output of estimated area is a double
  expect_type(newcommer_recom(B = 1000)$estimated_area, "double")
  #checking that output of points is a list
  expect_type(newcommer_recom(B = 1000)$points, "list")
  #checking that general output of the function is a list
  expect_type(newcommer_recom(B = 1000), "list")

})
