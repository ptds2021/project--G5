# Project in Programming tools - Group 5

##  Motivation
An anime is a hand-drawn or computer animation originated from Japan. Anime describes all animated work, regardless of the style or origin. The anime industry consists of over 430 production companies, including major studios like Studio Ghibli, Sunrise, and Toei Animation. As of 2016, Japanese anime accounted for 60 percent of the world’s animated television shows. 
It could be helpful for someone who has several criteria to have recommendation for anime. This could save time for the viewers while offering them an anime in line with their expectations.

## Goal
The goal of our project is to make the user of our application fill in a certain number of boxes in order to offer him the best anime recommendation: We can ask him which type of anime he prefers ( i.e. actions, romance). We can also ask him to give us some indications about the time he has to watch it. Finally, with their age, we can also better target our recommendation thanks to the `Rating` variable.

## Data

To create the data we have into the package, we downloaded some from Kaggle [Anime dataset](https://www.kaggle.com/hernan4444/anime-recommendation-database-2020?select=watching_status.csv). We downloaded 2 tables, `anime` and `animelist`. Note that those are not present on the GitHub, and there is a simple reason for this : `animelist` was 1.9 Gb and GitHub asks not to go over 1Gb for a given repository if possible. Therefore we chose not to put any of them in the GitHub since you would have to download at east one.

Those tables were then used, and you can check the code on how it was done in the `data-wrangling` article, to create 2 datasets that are included in the package:

1) `anime` containing the full anime list with some variables

2) `anime_with_ratings` containing anime_id and scores of the users kept for each of them

## Functions
In order to have a well-working shiny app, we constructed several functions:

There are 4 anime oriented: 

1) `newcommer_recom()`

2) `user_based_recom()`
                            
3) `user_item_matrix()`

4) `item_recommendation()`

And 5 generic ones that can be re-used easily

1) `selectize_count()`

2) `selectize_names ()`

3) `create_numeric_input ()`
                                 
4) `score_recovery()`

5) `cos_similarity()`

And finally, the one running the application :

1) `anime_finder()`

## Website and ShinyApp
In order to have access to the ShinyApp, you just have to run `ProjectG5::anime_finder()` in R. Or go to https://massimofinini.shinyapps.io/anime_finder/

Website info: ... 
