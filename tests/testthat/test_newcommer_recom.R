library(testthat)

#newcommer_recom
test_that("newcommer_recom input",{
  #age
  #checking that age is not character and that it does not work when it is
  expect_error(newcommer_recom(anime,
                               age = "text",
                               gender = "Action",
                               freetime = 60))
  #checking that age is not negative and that it does not work when it is
  expect_error(newcommer_recom(anime,
                               age = -1,
                               genre = "Sport",
                               freetime = 60))

  #freetime
  #checking that free time is not negative and that it does not work when it is
  expect_error(newcommer_recom(anime,
                               age = 15,
                               gender = "Action",
                               freetime = -1))
  #checking that free time is not character and that it does not work when it is
  expect_error(newcommer_recom(anime,
                               age = 15,
                               gender = "Action",
                               freetime = "text"))
})

test_that("newcommer_recom output",{
  #checking the output has the right type
  expect_type(newcommer_recom(anime,
                              age = 20,
                              gender = "action",
                              freetime = 60),
              "list")
})


