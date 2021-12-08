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






exper_others_recom = function(userid,
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
