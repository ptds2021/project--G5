# Project in Programming tools - Group 5

##  Motivation
An anime is a hand-drawn or computer animation originated from Japan. Anime describes all animated work, regardless of the style or origin. The anime industry consists of over 430 production companies, including major studios like Studio Ghibli, Sunrise, and Toei Animation. As of 2016, Japanese anime accounted for 60 percent of the world’s animated television shows. 
It could be helpful for someone who has several criteria to have recommendation for anime. This could save time for the viewers while offering them an anime in line with their expectations.

## Goal
The goal of our project is to make the user of our application fill in a certain number of boxes in order to offer him the best anime recommendation: We can ask him which type of anime he prefers ( i.e. actions, romance). We can also ask him to give us some indications about the time he has to watch it. Finally, with their age, we can also better target our recommendation thanks to the `Rating` variable.

## Functions
In order to have a well-working shiny app, we constructed several functions:

4 anime oriented 

FUNCTION newcommer_recom( ANIME, AGE, GENDERS, FREETIME )

FUNCTION user_based_recom(userid = 999999999 ,
                            user_item_matrix ,
                            ratings_matrix ,
                            n_recommendation = 5,
                            threshold = 1,
                            nearest_neighbors = 10)
                            
FUNCTION user_item_matrix(data = anime_with_ratings, adding_row = FALSE, row_data = NULL)

FUNCTION item_recommendation(selected_item_name, rating_matrix, n_recommendation, data)

5 generic ones

FUNCTION selectize_count (id)

FUNCTION selectize_names (id)

FUNCTION create_numeric_input (selectednames,
                                 selectedcounts,
                                 min_user = 1,
                                 max_user = 10,
                                 placeholder = 5,
                                 wanted_step = 0.5)
                                 
FUNCTION score_recovery(selectedcounts, input)

FUNCTION cos_similarity( A, B)

## Website and ShinyApp
In order to have access to the ShinyApp, you just have to run `ProjectG5::anime_finder()` in R.

Website info: ... 
