\name{getPositionFromHover}
\alias{getPositionFromHover}
\title{
Get the position of hovered point on the heatmap image
}
\description{
Get the position of hovered point on the heatmap image
}
\usage{
getPositionFromHover(hover, ratio = 1)
}
\arguments{

  \item{hover}{The input hover object. Assume \code{heatmap_hover} is the ID set to argument \code{hover} in \code{\link[shiny]{plotOutput}}, then the value here is \code{input$heatmap_hover}.}
  \item{ratio}{The relative resolution. The value should the ratio between \code{res} set in \code{\link{makeInteractiveComplexHeatmap}} and 72 (\code{res/72}).}

}
\value{
A \code{\link[grid]{unit}} object of length two which are the coordinates of the hover points.
}
\examples{
# There is no example
NULL

}
