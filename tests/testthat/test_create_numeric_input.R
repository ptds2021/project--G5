library(testthat)

test_that("create_numeric_input input",{
  #min_user
  #checking that min_user is not character and that it does not work when it is
  expect_error(create_numeric_input(min_user = "text",
                                    selectedcounts = 2,
                                    selectednames = c("a", "b"),
                                    id = "hello"
                                    ))
  #checking that min_user is not negative and that it does not work when it is
  expect_error(create_numeric_input(min_user = -1,
                                    selectedcounts = 2,
                                    selectednames = c("a", "b"),
                                    id = "hello"))

  #max_user
  #checking that max_user is not character and that it does not work when it is
  expect_error(create_numeric_input(max_user = "text",
                                    selectedcounts = 2,
                                    selectednames = c("a", "b"),
                                    id = "hello"))
  #checking that min_user is not negative and that it does not work when it is
  expect_error(create_numeric_input(max_user = -1,
                                    selectedcounts = 2,
                                    selectednames = c("a", "b"),
                                    id = "hello"))

  #placeholder
  #checking that placeholder is not character and that it does not work when it is
  expect_error(create_numeric_input(placeholder = "hello",
                                    selectedcounts = 2,
                                    selectednames = c("a", "b"),
                                    id = "hello"))

  #wanted_step
  #checking that wanted_step is not character and that it does not work when it is
  expect_error(create_numeric_input(wanted_step = "hi",
                                    selectedcounts = 2,
                                    selectednames = c("a", "b"),
                                    id = "hello"))
  #checking that wanted_step is not null and that it does not work when it is
  expect_error(create_numeric_input(wanted_step = 0,
                                    selectedcounts = 2,
                                    selectednames = c("a", "b"),
                                    id = "hello"))

  #selectednames
  #checking that selected names is not numeric and that it does not work when it is
  expect_error(create_numeric_input(selectedcounts = 2,
                                    selectednames = c(1, 2),
                                    id = "hello"))

  #selectedcount
  #checking that selectedcount is not character and that it does not work when it is
  expect_error(create_numeric_input(selectedcounts = "hi",
                                    selectednames = c("a", "b"),
                                    id = "hello"))
  #checking that selectedcount is not negative and that it does not work when it is
  expect_error(create_numeric_input(selectedcounts = -2,
                                    selectednames = c("a", "b"),
                                    id = "hello"))

})



test_that("create_numeric_input output",{
  #checking that the function output is a valid list
  expect_type(create_numeric_input(selectedcounts = 2,
                                    selectednames = c("a", "b"),
                                    id = "hello"), "list")
})

