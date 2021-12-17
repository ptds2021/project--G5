\name{anime}
\alias{anime}
\docType{data}
\title{
  Detailed Description for each anime
}
\description{
  These table shows for each anime its gender, duration, popularity, rating and many other descriptive variables.
}
\usage{data("anime")}
\format{
  A data frame with 8155 observations on the following 25 variables.
  \describe{
    \item{\code{item_id}}{a character vector}
    \item{\code{Name}}{a character vector}
    \item{\code{Score}}{a character vector}
    \item{\code{Genders}}{a character vector}
    \item{\code{english_name}}{a character vector}
    \item{\code{Type}}{a character vector}
    \item{\code{Episodes}}{a character vector}
    \item{\code{Aired}}{a character vector}
    \item{\code{Premiered}}{a character vector}
    \item{\code{Producers}}{a character vector}
    \item{\code{Licensors}}{a character vector}
    \item{\code{Studios}}{a character vector}
    \item{\code{Source}}{a character vector}
    \item{\code{Duration}}{a numeric vector}
    \item{\code{Duration_C}}{a factor with levels \code{Less than 30min} \code{Less than 1h} \code{more than 1h} \code{more than 2h}}
    \item{\code{Rating}}{a character vector}
    \item{\code{Ranked}}{a character vector}
    \item{\code{Popularity}}{a numeric vector}
    \item{\code{Members}}{a numeric vector}
    \item{\code{Favorites}}{a numeric vector}
    \item{\code{Watching}}{a numeric vector}
    \item{\code{Completed}}{a numeric vector}
    \item{\samp{On-Hold}}{a numeric vector}
    \item{\code{Dropped}}{a numeric vector}
    \item{\samp{Plan to Watch}}{a numeric vector}
  }
}
\details{
 We don't use all the column
}
\source{
  Thise data was first obtained on the website Kaggle, which was upload thanks to the site MyAnimeList
}
\references{

}
\examples{
data(anime)
## maybe str(anime) ; plot(anime) ...
}
\keyword{datasets}
