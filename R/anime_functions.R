#'@title Filter anime according to preferences
#'
#'@description Filter the anime list depending on different variables. These variables must be included in the data set.
#'
#'@author Marie Bellier, Massimo Finini, Meri Likoska, Vania Rodrigues Telo Ramos, Xavier Renger
#'
#'@param data data containing the list of anime with the variables being description for each anime
#'@param age age given by the person
#'@param gender theme of the anime. For instance if you like action anime, put "action"
#'@param freetime free time that the person has to use in order to watch the anime
#'
#'@return Return a filtered table with the anime depending on the inputs
#'
#'@export

newcommer_recom <- function(data, age, gender, freetime){
  # Controls
  if (!is.numeric(age)) {
    stop("Argument age is not valid. It must be a number.")
  }
  if (age <= 0) {
    stop("Argument age is not valid. It must be positive.")
  }
  if (!is.numeric(freetime)) {
    stop("Argument freetime is not valid. It must be a number.")
  }
  if (freetime <= 0) {
    stop("Argument freetime is not valid. It must be positive.")
  }

  if  (age < 13) {
    newdata <- data %>%
      filter(Rating == 'G - All Ages')
  } else if (age >= 13 & age <= 16) {
    newdata <- data %>%
      filter(Rating == 'G - All Ages' |
               Rating == 'PG-13 - Teens 13 or older')
  } else if (age > 16 & age < 18) {
    newdata <- data %>%
      filter(
        Rating == 'G - All Ages' |
          Rating == 'R+ - Mild Nudity' |
          Rating == 'R - 17+ (violence & profanity)'
      )
  } else {
    newdata <- data
  }

  finaldata = newdata %>%
    filter(str_detect(newdata$Genders, gender)) %>%
    filter(Duration <= freetime)%>%
    select(Name, Genders, Type, Episodes, Duration, Rating, Popularity) %>%
    arrange(Popularity)



    return(finaldata)
}

#'@title Compute the cosine similarity between two objects A and B
#'
#'@author Marie Bellier, Massimo Finini, Meri Likoska, Vania Rodrigues Telo Ramos, Xavier Renger
#'
#'@param A first object
#'@param B second object
#'
#'@return Return the cosine similarity value
#'
#'@export

cos_similarity = function(A,B){
  num = sum(A *B, na.rm = T)
  den = sqrt(sum(A^2, na.rm = T)) * sqrt(sum(B^2, na.rm = T))
  result = num/den

  return(result)
}



#'@title Similarity between other users to have a list of anime that the user have not seen yet
#'
#'@description Use the user item matrix to search for similar users in order to get the anime they graded the best and that we have not watched
#'
#'@author Marie Bellier, Massimo Finini, Meri Likoska, Vania Rodrigues Telo Ramos, Xavier Renger
#'
#'@param userid user id, set to 999999999 so that there is no conflict with other Id's
#'@param user_item_matrix matrix created by the function `user_item_matrix`
#'@param ratings_matrix table of ratings similar to the one we kept
#'@param n_recommendation number of recommendations wanted
#'@param threshold threshold level of number of user that gave a score to the anime
#'@param nearest_neighbors number of neighbors taken into account for the computation
#'
#'@return Return a table composed of `n_recommendation` that the user have not seen yet
#'
#'@import utils
#'
#'
#'@export


user_based_recom = function(userid = 999999999 ,
                            user_item_matrix ,
                            ratings_matrix ,
                            n_recommendation = 5,
                            threshold = 1,
                            nearest_neighbors = 10) {

  # Controls
  if (!is.numeric(userid)) {
    stop("Argument userid is not valid. It must be a number.")
  }
  if (!is.numeric(n_recommendation)) {
    stop("Argument n_recommendation is not valid. It must be a number.")
  }

  user_index = which(rownames(user_item_matrix) == userid)

  similarity = apply(
    user_item_matrix,
    1,
    FUN = function(y)
      ProjectG5::cos_similarity(user_item_matrix[user_index, ], y)
  )

  similar_users = tibble(user_id = names(similarity),
                         similarity = similarity) %>%
    filter(user_id != userid) %>%
    arrange(desc(similarity)) %>%
    top_n(nearest_neighbors, similarity)


  watched_anime_user = ratings_matrix$item_id[ratings_matrix$user_id == userid]

  recommendations = ratings_matrix %>%
    filter(user_id %in% similar_users$user_id &
             !(item_id %in% watched_anime_user)) %>%
    group_by(item_id, Name, Episodes, Duration) %>%
    summarise(count = n(),
              rating = mean(rating)) %>%
    filter(count > threshold) %>%
    arrange(desc(rating), desc(count)) %>%
    utils::head(n_recommendation)

  return(recommendations)

}

#'@title Score per anime per user
#'
#'@description Create the user_item matrix from the given data. We use `adding_row = TRUE` for the user based recommendation to be able to add the user selected data to the table
#'
#'@author Marie Bellier, Massimo Finini, Meri Likoska, Vania Rodrigues Telo Ramos, Xavier Renger
#'
#'@param data data having the user ratings per anime
#'@param anime_with_ratings the data by default
#'@param adding_row Variable that is either TRUE if you want the matrix for the user-based recommendation or FALSE if you want the matrix for the item-based recommendation
#'@param row_data table created by the function `user_data()`
#'
#'@return Return a matrix where the x axis include the item_id and the y axis include the user_id. Each line is the score per item per user
#'
#'
#'@export


user_item_matrix <- function(data = anime_with_ratings, adding_row = FALSE, row_data = NULL){

  if(adding_row == TRUE){

    user_item <- data %>%
      bind_rows(row_data)%>%
      select(item_id,rating, user_id)%>%
      pivot_wider(names_from = item_id, values_from = rating) %>%
      as.data.frame()

    row.names(user_item) = user_item$user_id
    user_item$user_id = NULL

    user_item_matrix = as.matrix(user_item)

    return(user_item_matrix)
  }

  else if(adding_row == FALSE){

    user_item <- data %>%
      select(item_id, rating, user_id)%>%
      pivot_wider(names_from = item_id, values_from = rating) %>%
      as.data.frame()

    row.names(user_item) = user_item$user_id
    user_item$user_id = NULL

    user_item_matrix = as.matrix(user_item)

    return(user_item_matrix)
  }
}





#'@title Similarity between anime
#'
#'@description Use the user item matrix to search for similar anime
#'
#'@author Marie Bellier, Massimo Finini, Meri Likoska, Vania Rodrigues Telo Ramos, Xavier Renger
#'
#'@param selected_item_name selection of the user in the Shiny App
#'@param user_item_matrix matrix created by the function `user_item_matrix`
#'@param n_recommendation number of recommendation wanted by the user
#'@param data table of all anime
#'
#'@return Return a table composed of `n_recommendation` that the user have not seen yet
#'
#'
#'@export


item_recommendation = function(selected_item_name,
                               user_item_matrix,
                               n_recommendation,
                               data){

  # Controls
  if (!is.numeric(n_recommendation)) {
    stop("Argument n_recommendation is not valid. It must be a number.")
  }

  item_picked <- data %>% filter(Name == selected_item_name)

  selected_item_id <- item_picked$item_id

  item_index = which(colnames(user_item_matrix) == selected_item_id)

  similarity = apply(user_item_matrix, 2, FUN = function(y)
    ProjectG5::cos_similarity(user_item_matrix[,item_index], y))

  recommendations = tibble(item_id = names(similarity),
                           similarity = similarity) %>%
    filter(item_id != selected_item_id) %>%
    top_n(n_recommendation, similarity) %>%
    arrange(desc(similarity)) %>%
    left_join(anime, by= "item_id") %>%
    select(item_id, Name, Episodes, Duration, similarity)

  return(recommendations)

}



