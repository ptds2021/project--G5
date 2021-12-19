library(testthat)

#item_recommendation
test_that("item_recommendation input",{
  #selected_item_name
  #checking that selected_item_name is not numeric and that it does not work when it is
  expect_error(item_recommendation(selected_item_name = 1,
                                   user_item_matrix(ProjectG5::anime_with_ratings),
                                   n_recommendation = 5,
                                   data = ProjectG5::anime))

  #n_recommendation
  #checking that n_recommendation is not character and that it does not work when it is
  expect_error(item_recommendation(selected_item_name = "Naruto",
                                   user_item_matrix(ProjectG5::anime_with_ratings),
                                   n_recommendation = "test",
                                   data= ProjectG5::anime))

})

test_that("item_recommendation output",{
  #checking that age is not character and that it does not work when it is
  expect_type(item_recommendation(selected_item_name = "Naruto",
                                  user_item_matrix(ProjectG5::anime_with_ratings),
                                  n_recommendation = 5,
                                  data = ProjectG5::anime), "list")
})


