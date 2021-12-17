library(testthat)
test_that("create_numeric_input input",{
  #checking that min_user is not negative and that it does not work when it is
  expect_error(create_numeric_input(min_user = -1))
  #checking that max_user is not character and that it does not work when it is
  expect_error(create_numeric_input(max_user = "text"))
})

