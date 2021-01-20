\name{htShiny}
\alias{htShiny}
\title{
Interactive heatmaps with a shiny app
}
\description{
Interactive heatmaps with a shiny app
}
\usage{
htShiny(ht_list = get_last_ht(), title = NULL, description = NULL,
    hline = TRUE, html = NULL, ...)
}
\arguments{

  \item{ht_list}{A \code{\link[ComplexHeatmap]{Heatmap-class}} or a \code{\link[ComplexHeatmap]{HeatmapList-class}} object. If it is not specified, a random heatmap is used. Better already updated by \code{draw()} function.}
  \item{title}{Title of the app.}
  \item{description}{Description of the app.}
  \item{hline}{Whether to add the horizontal line (by \code{hr} tag).}
  \item{html}{HTML fragment inserted below the heatmap.}
  \item{...}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}

}
\seealso{
\url{https://jokergoo.shinyapps.io/interactive_complexHeatmap/}
}
\value{
A shiny app object.
}
\examples{
# use a random heatmap
if(interactive()) {
    htShiny()
}

# by providing a heatmap/heatmap list
if(interactive()) {
    m = matrix(rnorm(100), 10)
    rownames(m) = 1:10
    colnames(m) = 1:10

    ht = Heatmap(m)
    ht = draw(ht)
    htShiny(ht)
}

if(interactive()) {
    m1 = matrix(rnorm(100), 10)
    rownames(m1) = 1:10
    colnames(m1) = 1:10
    ht1 = Heatmap(m1, row_km = 2, column_km = 2)

    m2 = matrix(sample(letters[1:10], 100, replace = TRUE), 10)
    ht2 = Heatmap(m2)

    ht_list = draw(ht1 + ht2)
    htShiny(ht_list)

    ht_list = ht1 \%v\% ht2
    htShiny(ht_list)
}
}
