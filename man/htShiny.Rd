\name{htShiny}
\alias{htShiny}
\title{
Interactive heatmaps with a Shiny app
}
\description{
Interactive heatmaps with a Shiny app
}
\usage{
htShiny(ht_list = get_last_ht(), title = NULL,
    description = NULL, hline = TRUE, html = NULL,
    
    # parameters passed to InteractiveComplexHeatmapOutput()
    heatmap_id = NULL, title1 = "Original heatmap", title2 = "Selected sub-heatmap",
    width1 = ifelse(layout \%in\% c("1|(2+3)", "1|23"), 800, 450),
    height1 = ifelse(layout \%in\% c("1+(2|3)"), 700, 350),
    width2 = 370,
    height2 = 350,
    width3 = ifelse(layout \%in\% c("(1+2)|3", "12|3"), 800, 370),
    layout = "(1+2)|3",
    action = "click", brush_opt = list(stroke = "#f00", opacity = 0.6)
    )
}
\arguments{

  \item{ht_list}{A \code{\link[ComplexHeatmap]{Heatmap-class}} or a \code{\link[ComplexHeatmap]{HeatmapList-class}} object. If it is not specified, the last generated heatmap is used. Better already updated by \code{draw()} function.}
  \item{title}{Title of the app.}
  \item{description}{Description of the app.}
  \item{hline}{Whether to add the horizontal line (by \code{hr} tag).}
  \item{html}{HTML fragment inserted below the heatmap.}
  \item{heatmap_id}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{title1}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{title2}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{width1}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{height1}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{width2}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{height2}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{width3}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{layout}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{action}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{brush_opt}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}

}
\seealso{
\url{https://jokergoo.shinyapps.io/interactive_complexHeatmap/}
}
\value{
A Shiny app object.
}
\examples{
# use last generated heatmap
if(interactive() && dev.interactive()) {
    m = matrix(rnorm(100), 10)
    Heatmap(m)
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
