\name{htPositionsOnDevice}
\alias{htPositionsOnDevice}
\title{
Get heatmap positions on the graphics device
}
\description{
Get heatmap positions on the graphics device
}
\usage{
htPositionsOnDevice(ht_list = get_last_ht(), unit = "inch", valueOnly = FALSE,
    include_annotation = FALSE, calibrate = TRUE)
}
\arguments{

  \item{ht_list}{A \code{\link[ComplexHeatmap]{HeatmapList-class}} object returned by \code{\link[ComplexHeatmap:draw-Heatmap-method]{draw,Heatmap-method}} or \code{\link[ComplexHeatmap:draw-HeatmapList-method]{draw,HeatmapList-method}}. If it is omitted, it uses the last generated heatmap.}
  \item{unit}{The unit.}
  \item{valueOnly}{Whether only return the numeric values.}
  \item{include_annotation}{Internally used.}
  \item{calibrate}{Internally used.}

}
\details{
\code{ht_list} must have been already updated by \code{draw()} function. The function needs to be executed under a graphics device where the heatmap is written.
}
\value{
It returns a \code{\link[S4Vectors:DataFrame-class]{DataFrame}} object of the position of every heatmap slice.
}
\examples{
if(dev.interactive()) {
    m = matrix(rnorm(100), 10)
    ht = Heatmap(m, row_km = 2, column_km = 2)
    ht = draw(ht)
    pos = htPositionsOnDevice(ht)

    InteractiveComplexHeatmap:::redraw_ht_vp(pos)
}
}
