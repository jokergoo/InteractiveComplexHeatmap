\name{MakeInteractiveComplexHeatmap}
\alias{MakeInteractiveComplexHeatmap}
\title{
Process the heatmaps in the sever side
}
\description{
Process the heatmaps in the sever side
}
\usage{
MakeInteractiveComplexHeatmap(ht_list, input, output, session, heatmap_id = shiny_env$current_heatmap_id)
}
\arguments{

  \item{ht_list}{A \code{\link[ComplexHeatmap]{Heatmap-class}} or a \code{\link[ComplexHeatmap]{HeatmapList-class}} object.}
  \item{input}{Passed from the shiny server function.}
  \item{output}{Passed from the shiny server function.}
  \item{session}{Passed from the shiny server function.}
  \item{heatmap_id}{The corresponding heatmap ID from the UI. If there is only one interactive heatmap in the app,  this argument does not need to be specified and it will use the current one specified in UI.}

}
\examples{
if(interactive()) {
	ht = Heatmap(m)
	ht = draw(ht)

ui = fluidPage(
		ComplexHeatmapOutput()
	)

server = function(input, output, session) {
		MakeInteractiveComplexHeatmap(ht, input, output, session)
	}

shiny::shinyApp(ui, server)
}
}
