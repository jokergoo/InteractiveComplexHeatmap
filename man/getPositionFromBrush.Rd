\name{getPositionFromBrush}
\alias{getPositionFromBrush}
\title{
Get the position of the brushed area on the heatmap image
}
\description{
Get the position of the brushed area on the heatmap image
}
\usage{
getPositionFromBrush(brush, ratio = 1)
}
\arguments{

  \item{brush}{The input brush object. Assume \code{heatmap_brush} is the value set to argument \code{brush} in \code{\link[shiny]{plotOutput}}, then the value here is \code{input$heatmap_brush}.}
  \item{ratio}{The relative resolution. The value should the ratio between \code{res} set in \code{\link{makeInteractiveComplexHeatmap}} and 72 (\code{res/72}).}

}
\value{
A list of length two. The two elements correspons to the coordinates of the two diagonal points.
}
\seealso{
\code{\link{getPositionFromClick}}, \code{\link{getPositionFromHover}}, \code{\link{getPositionFromDblclick}}.
}
\examples{
# There is no example
NULL
}
