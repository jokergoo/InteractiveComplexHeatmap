\name{makeInteractiveComplexHeatmap}
\alias{makeInteractiveComplexHeatmap}
\title{
Process heatmaps on the sever side
}
\description{
Process heatmaps on the sever side
}
\usage{
makeInteractiveComplexHeatmap(input, output, session, ht_list,
    heatmap_id = shiny_env$current_heatmap_id,
    click_action = NULL, hover_action = NULL,
    dblclick_action = NULL, brush_action = NULL, res = 72)
}
\arguments{

  \item{input}{Passed from the Shiny server function.}
  \item{output}{Passed from the Shiny server function.}
  \item{session}{Passed from the Shiny server function.}
  \item{ht_list}{A \code{\link[ComplexHeatmap]{Heatmap-class}} or a \code{\link[ComplexHeatmap]{HeatmapList-class}} object.}
  \item{heatmap_id}{The corresponding heatmap ID from the UI. If there is only one interactive heatmap in the app,  this argument does not need to be specified and it will use the current one specified in \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{click_action}{Additional action at the server side when receiving a click event on the UI.}
  \item{hover_action}{Additional action at the server side when receiving a hover event on the UI.}
  \item{dblclick_action}{Additional action at the server side when receiving a dblclick event on the UI.}
  \item{brush_action}{Additional action at the server side when receiving a brush event on the UI.}
  \item{res}{Resolution of the plot.}

}
\value{
No value is returned.
}
\examples{
if(interactive()) {
    ht = Heatmap(m)
    ht = draw(ht)

    ui = fluidPage(
        InteractiveComplexHeatmapOutput()
    )

    server = function(input, output, session) {
        makeInteractiveComplexHeatmap(input, output, session, ht)
    }

    shiny::shinyApp(ui, server)
}
}
