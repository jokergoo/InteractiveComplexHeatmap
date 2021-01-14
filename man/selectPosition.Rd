\name{selectPosition}
\alias{selectPosition}
\title{
Select a position in the heatmap
}
\description{
Select a position in the heatmap
}
\usage{
selectPosition(ht_list = get_last_ht(), pos = NULL, mark = TRUE, verbose = TRUE,
    ht_pos = NULL, calibrate = TRUE)
}
\arguments{

  \item{ht_list}{A \code{\link[ComplexHeatmap]{HeatmapList-class}} object returned by \code{\link[ComplexHeatmap:draw-Heatmap-method]{draw,Heatmap-method}} or \code{\link[ComplexHeatmap:draw-HeatmapList-method]{draw,HeatmapList-method}}.}
  \item{mark}{Whether to mark the selected position as a point.}
  \item{pos}{If the value is \code{NULL}, it can be selected by click on the heatmap (of cource, the heatmap should be on the interactive graphic device). If it is set, it must be a \code{\link[grid]{unit}} object with length two which corresponds to the x and y position of the point.}
  \item{verbose}{Whether to print messages.}
  \item{ht_pos}{A value returned by \code{\link{htPositionsOnDevice}}.}
  \item{calibrate}{Internally used.}

}
\value{
A \code{\link[S4Vectors:DataFrame-class]{DataFrame}} object with row indices and column indices corresponding to the selected position.
}
\examples{
if(dev.interactive()) {
m = matrix(rnorm(100), 10)
rownames(m) = 1:10
colnames(m) = 1:10

ht = Heatmap(m)
ht = draw(ht)
selectPosition(ht)
}
}
