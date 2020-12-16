\name{ht_shiny}
\alias{ht_shiny}
\title{
Interactive heatmaps with a shiny app
}
\description{
Interactive heatmaps with a shiny app
}
\usage{
ht_shiny(ht_list, ..., html = NULL)
}
\arguments{

  \item{ht_list}{A \code{\link[ComplexHeatmap]{Heatmap-class}} or a \code{\link[ComplexHeatmap]{HeatmapList-class}} object. If it is not specified, a random heatmap is used. Better be updated by \code{draw()} function.}
  \item{...}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{html}{HTML fragment inserted below the heatmap.}

}
\seealso{
\url{https://jokergoo.shinyapps.io/interactive_complexHeatmap/}
}
\examples{
# use a random heatmap
if(interactive()) {
ht_shiny()
}

# by providing a heatmap/heatmap list
if(interactive()) {
m = matrix(rnorm(100), 10)
rownames(m) = 1:10
colnames(m) = 1:10

ht = Heatmap(m)
ht = draw(ht)
ht_shiny(ht)
}

if(interactive()) {
m1 = matrix(rnorm(100), 10)
rownames(m1) = 1:10
colnames(m1) = 1:10
ht1 = Heatmap(m1, row_km = 2, column_km = 2)

m2 = matrix(sample(letters[1:10], 100, replace = TRUE), 10)
ht2 = Heatmap(m2)

ht_list = draw(ht1 + ht2)
ht_shiny(ht_list)

ht_list = ht1 \%v\% ht2
ht_shiny(ht_list)
}
}
