\name{ht_pos_on_device}
\alias{ht_pos_on_device}
\title{
Get the heatmap positions on the graphic device
}
\description{
Get the heatmap positions on the graphic device
}
\usage{
ht_pos_on_device(ht_list, unit = "inch", valueOnly = FALSE,
    include_annotation = FALSE, calibrate = TRUE)
}
\arguments{

  \item{ht_list}{A \code{\link[ComplexHeatmap]{HeatmapList-class}} object returned by \code{\link[ComplexHeatmap:draw-Heatmap-method]{draw,Heatmap-method}} or \code{\link[ComplexHeatmap:draw-HeatmapList-method]{draw,HeatmapList-method}}.}
  \item{unit}{The unit.}
  \item{valueOnly}{Whether only return the numeric values.}
  \item{include_annotation}{Internally used.}
  \item{calibrate}{Internally used.}

}
\value{
It returns a \code{\link[S4Vectors:DataFrame-class]{DataFrame}} object of the position of every heatmap slice.
}
\examples{
if(dev.interactive()) {
m = matrix(rnorm(100), 10)
ht = Heatmap(m, row_km = 2, column_km = 2)
ht = draw(ht)
pos = ht_pos_on_device(ht)

InteractiveComplexHeatmap:::redraw_ht_vp(pos)
}
}
