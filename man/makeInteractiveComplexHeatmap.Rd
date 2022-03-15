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
    dblclick_action = NULL, brush_action = NULL, res = 72,
    show_cell_fun = NULL, show_layer_fun = NULL)
}
\arguments{

  \item{input}{Passed from the Shiny server function.}
  \item{output}{Passed from the Shiny server function.}
  \item{session}{Passed from the Shiny server function.}
  \item{ht_list}{A \code{\link[ComplexHeatmap]{Heatmap-class}} or a \code{\link[ComplexHeatmap]{HeatmapList-class}} object.}
  \item{heatmap_id}{The corresponding heatmap ID from the UI. If there is only one interactive heatmap in the app,  this argument does not need to be specified and it will use the current one used in \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{click_action}{Additional actions on the server side when receiving a click event on the UI. This self-defined function should accept two or four arguments. If it is two arguments, they should be \code{df} and \code{output} and if it is four arguments, they should be \code{df}, \code{input}, \code{output} and \code{session}.}
  \item{hover_action}{Additional actions at the server side when receiving a hover event on the UI.}
  \item{dblclick_action}{Additional actions at the server side when receiving a dblclick event on the UI.}
  \item{brush_action}{Additional actions at the server side when receiving a brush event on the UI.}
  \item{res}{Resolution of the plot, pass to \code{\link[shiny]{renderPlot}}.}
  \item{sub_heatmap_cell_fun}{The \code{cell_fun} specifically defined for sub-heatmap.}
  \item{sub_heatmap_layer_fun}{The \code{layer_fun} specifically defined for sub-heatmap.}
  \item{show_cell_fun}{Whether show graphics made by \code{cell_fun} on the main heatmap?}
  \item{show_layer_fun}{Whether show graphics made by \code{cell_fun} on the main heatmap?}

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
