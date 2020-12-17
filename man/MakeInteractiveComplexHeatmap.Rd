\name{MakeInteractiveComplexHeatmap}
\alias{MakeInteractiveComplexHeatmap}
\title{
Process the heatmaps on the sever side
}
\description{
Process the heatmaps on the sever side
}
\usage{
MakeInteractiveComplexHeatmap(ht_list, input, output, session,
    heatmap_id = shiny_env$current_heatmap_id,
    click_action = NULL, brush_action = NULL,
    default_click_action = TRUE, default_brush_action = TRUE)
}
\arguments{

  \item{ht_list}{A \code{\link[ComplexHeatmap]{Heatmap-class}} or a \code{\link[ComplexHeatmap]{HeatmapList-class}} object.}
  \item{input}{Passed from the shiny server function.}
  \item{output}{Passed from the shiny server function.}
  \item{session}{Passed from the shiny server function.}
  \item{heatmap_id}{The corresponding heatmap ID from the UI. If there is only one interactive heatmap in the app,  this argument does not need to be specified and it will use the current one specified in \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{click_action}{The action at the sever side when receiving a click event on the UI. If \code{action} is selected as \code{hover} or \code{dblclick} in \code{\link{InteractiveComplexHeatmapOutput}}, then this argument controls the action for the hover or dblclick event.}
  \item{brush_action}{The action at the sever side when receiving a brush event on the UI.}
  \item{default_click_action}{Whether to apply the default click action on the sever side.}
  \item{default_brush_action}{Whether to apply the default brush action on the sever side.}

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
    MakeInteractiveComplexHeatmap(ht, input, output, session)
}

shiny::shinyApp(ui, server)
}
}
