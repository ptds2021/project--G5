library(testthat)

#newcommer_recom
test_that("newcommer_recom input",{
  #checking that age is not character and that it does not work when it is
  expect_error(newcommer_recom(anime,
                               age = "text",
                               gender = "action",
                               freetime = 60))
  #checking that free time is not negative and that it does not work when it is
  expect_error(newcommer_recom(freetime = -1))
  #checking that free time is not character and that it does not work when it is
  expect_error(newcommer_recom(freetime = "text"))
})

test_that("newcommer_recom output",{
  #checking that age is not character and that it does not work when it is
  expect_type(newcommer_recom(anime,
                              age = 20,
                              gender = "action",
                              freetime = 60),
              "list")
})


