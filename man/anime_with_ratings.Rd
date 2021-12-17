\name{anime_with_ratings}
\alias{anime_with_ratings}
\docType{data}
\title{
   Anime data with individual scores of viewers
}
\description{
  Each anime is scored, and here we have the indiviual scores given for each anime.
}
\usage{data("anime_with_ratings")}
\format{
  A data frame with 573588 observations on the following 7 variables.
  \describe{
    \item{\code{item_id}}{a character vector}
    \item{\code{user_id}}{a numeric vector}
    \item{\code{Name}}{a character vector}
    \item{\code{english_name}}{a character vector}
    \item{\code{Episodes}}{a character vector}
    \item{\code{Duration}}{a numeric vector}
    \item{\code{rating}}{a numeric vector}
  }
}

\source{
 Thise data was first obtained on the website Kaggle, which was upload thanks to the site MyAnimeList
}

\examples{
data(anime_with_ratings)
## maybe str(anime_with_ratings) ; plot(anime_with_ratings) ...
}
\keyword{datasets}
