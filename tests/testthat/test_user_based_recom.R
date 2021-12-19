library(testthat)

#score_recovery
test_that("score_recovery input",{
  #userid
  #checking that userid is not numeric and that it does not work when it is
  expect_error(user_based_recom(userid = "text" ,
                                user_item_matrix(ProjectG5::anime_with_ratings) ,
                                ratings_data ,
                                n_recommendation = 5,
                                threshold = 1,
                                nearest_neighbors = 10))
  #n_recommendation
  #checking that n_recommendation is not character and that it does not work when it is
  expect_error(user_based_recom(userid = 999999999 ,
                                user_item_matrix(ProjectG5::anime_with_ratings) ,
                                ratings_data ,
                                n_recommendation ="text",
                                threshold = 1,
                                nearest_neighbors = 10))

  #threshold
  #checking that threshold is not character and that it does not work when it is
  expect_error(user_based_recom(userid = 999999999 ,
                                user_item_matrix(ProjectG5::anime_with_ratings) ,
                                ratings_data ,
                                n_recommendation = 5,
                                threshold = "hello",
                                nearest_neighbors = 10))

  #nearest_neighbors
  #checking that nearest_neighbors is not character and that it does not work when it is
  expect_error(user_based_recom(userid = 999999999 ,
                                user_item_matrix(ProjectG5::anime_with_ratings) ,
                                ratings_data ,
                                n_recommendation = 5,
                                threshold = 1,
                                nearest_neighbors = "hello"))
})

#too complex to realize the output test
