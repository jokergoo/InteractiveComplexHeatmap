\name{getPositionFromDblclick}
\alias{getPositionFromDblclick}
\title{
Get the position of double clicked point on the heatmap image
}
\description{
Get the position of double clicked point on the heatmap image
}
\usage{
getPositionFromDblclick(dblclick, ratio = 1)
}
\arguments{

  \item{dblclick}{The input dblclick object. Assume \code{heatmap_dblclick} is the value set to argument \code{dblclick} in \code{\link[shiny]{plotOutput}}, then the value here is \code{input$heatmap_dblclick}.}
  \item{ratio}{The relative resolution. The value should the ratio between \code{res} set in \code{\link{makeInteractiveComplexHeatmap}} and 72 (\code{res/72}).}

}
\value{
A \code{\link[grid]{unit}} object of length two which are the coordinates of the double clicked points.
}
\examples{
# There is no example
NULL
}
