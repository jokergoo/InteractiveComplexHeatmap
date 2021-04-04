\name{getPositionFromClick}
\alias{getPositionFromClick}
\title{
Get the position of clicked point on the heatmap image
}
\description{
Get the position of clicked point on the heatmap image
}
\usage{
getPositionFromClick(click, ratio = 1)
}
\arguments{

  \item{click}{The input click object. Assume \code{heatmap_click} is the ID set to argument \code{click} in \code{\link[shiny]{plotOutput}}, then the value here is \code{input$heatmap_click}.}
  \item{ratio}{The relative resolution. The value should the ratio between \code{res} set in \code{\link{makeInteractiveComplexHeatmap}} and 72 (\code{res/72}).}

}
\value{
A \code{\link[grid]{unit}} object of length two which are the coordinates of the clicked points.
}
\seealso{
\code{\link{getPositionFromBrush}}, \code{\link{getPositionFromHover}}, \code{\link{getPositionFromDblclick}}.
}
\examples{
# There is no example
NULL

}
