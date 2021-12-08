# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}


#'@title Filter the enime list depdnding on simple data
#'
#'@author Marie Bellier, Massimo Finini, Meri Likoska, Vania Rodrigues Telo Ramos, Xavier Renger
#'
#'@param anime data containing the anime list
#'@param age age given by the person
#'@param gender gender of interest from the anime data
#'@param freetime freetime that the person has to use for the anime
#'
#'@return Return a filtered table with the anime depending on the inputs
#'
#'
#'@export

newcommer_recom <- function(anime, age, gender, freetime){
  if (age < 13) {
    newdata <- anime %>%
      filter(Rating == 'G - All Ages')
  } else if (age >= 13 & age <= 16) {
    newdata <- anime %>%
      filter(Rating == 'G - All Ages' |
               Rating == 'PG-13 - Teens 13 or older')
  } else if (age > 16 & age < 18) {
    newdata <- anime %>%
      filter(
        Rating == 'G - All Ages' |
          Rating == 'R+ - Mild Nudity' |
          Rating == 'R - 17+ (violence & profanity)'
      )
  } else {
    newdata <- anime
  }

  finaldata = newdata %>%
    filter(str_detect(newdata$Genders, gender)) %>%
    filter(Duration <= freetime)



    return(finaldata)
}


#'@title Use user input and anime data to return recommendation based on
#'
#'@author Marie Bellier, Massimo Finini, Meri Likoska, Vania Rodrigues Telo Ramos, Xavier Renger
#'
#'@param user_viewed data containing the anime list
#'@param age age given by the person
#'@param gender gender of interest from the anime data
#'@param freetime freetime that the person has to use for the anime
#'
#'@return Return a filtered table with the anime depending on the inputs
#'
#'
#'@export



exper_others_recom = function(user_viewed,
                              user_item_matrix = user_item,
                              ratings_matrix = ratings,
                              n_recommendations = 5,
                              threshold = 1,
                              nearest_neighbors = 10) {

  user_index = which(rownames(user_item_matrix) == userid)

  similarity = apply(
    user_item_matrix,
    1,
    FUN = function(y)
      cos_similarity(user_item_matrix[user_index, ], y)
  )

  similar_users = tibble(user_id = names(similarity),
                         similarity = similarity) %>%
    filter(user_id != userid) %>%
    arrange(desc(similarity)) %>%
    top_n(nearest_neighbors, similarity)


  watched_anime_user = ratings_matrix$anime_id[ratings_matrix$user_id == userid]

  recommendations = ratings_matrix %>%
    filter(user_id %in% similar_users$user_id &
             !(anime_id %in% watched_anime_user)) %>%
    group_by(anime_id) %>%
    summarise(count = n(),
              rating = mean(rating)) %>%
    filter(count > threshold) %>%
    arrange(desc(rating), desc(count)) %>%
    head(n_recommendations)

  return(recommendations)

}



exp_content_base_recom <- function(viewed = selected_anime, dissim = dissimilarity_matrix, all_anim = animes, N_recom = 5){

  selected_anime_index <- which(colnames(dissimilarity_matrix) %in% viewed$anime_id)

  results <- data.frame(dissimilarity_matrix[, selected_anime_index],
                        recommended_anime = row.names(dissimilarity_matrix),
                        stringsAsFactors = FALSE)

  recom_content_based = results %>%
    pivot_longer(cols = c(-"recommended_anime") , names_to = "readed_anime",
                 values_to = "dissimilarity") %>%
    left_join(selected_anime, by = c("recommended_anime" = "anime_id"))%>%
    arrange(desc(dissimilarity)) %>%
    filter(recommended_anime != readed_anime) %>%
    filter(!is.na(rating) ) %>%
    mutate(
      similarity = 1 - dissimilarity,
      weighted_score = similarity * rating)%>%
    arrange(desc(weighted_score)) %>%
    filter(weighted_score>0) %>%
    group_by(recommended_anime) %>% slice(1) %>%
    top_n(N_recom, weighted_score) %>%
    left_join(animes, by = c("recommended_anime" = "anime_id"))

  return(recom_content_based)
}
