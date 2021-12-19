library(testthat)

#score_recovery
test_that("score_recovery input",{
  #id
  #checking that id is not numeric and that it does not work when it is
  expect_error(score_recovery(selectedcounts = 2,
                              input = input,
                              id = 60))
  #selectedcounts
  #checking that selectedcounts is not negative and that it does not work when it is
  expect_error(score_recovery(selectedcounts = -2,
                               input = input,
                               id = "hi"))
  #checking that selectedcounts is not character and that it does not work when it is
  expect_error(score_recovery(selectedcounts = "hello",
                               input = input,
                               id = "hi"))
})

#cannot test the output outside a shiny
