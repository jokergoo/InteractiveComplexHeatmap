\name{mainHeatmapOutput}
\alias{mainHeatmapOutput}
\title{
UI for the main complex heatmaps
}
\description{
UI for the main complex heatmaps
}
\usage{
mainHeatmapOutput(heatmap_id, title = NULL,
    width = 450, height = 350,
    action = "click", cursor = TRUE,
    response = c(action, "brush"),
    brush_opt = list(stroke = "#f00", opacity = 0.6),
    containment = FALSE)
}
\arguments{

  \item{heatmap_id}{ID of the plot.}
  \item{title}{Title of the original heatmap.}
  \item{width}{Width of the original heatmap.}
  \item{height}{Height of the original heatmap.}
  \item{action}{Which action for selecting single cell on the heatmap? Value should be \code{click}, \code{hover} or \code{dblclick}.}
  \item{cursor}{WHen moving mouse on heatmap, whether to show the cursors on the four sides?}
  \item{response}{Which action needs to be respond on the server side. Value should be in \code{click}/\code{hover}/\code{dblclick} and \code{brush}. Please note, if \code{brush} is not selected, there is no "search tool" in the main heatmap.}
  \item{brush_opt}{A list of parameters passed to \code{\link[shiny]{brushOpts}}. Do not set an ID for the brush. An internal brush ID is automatically set.}
  \item{containment}{If this output is inside a container, whether the resizing be restricted inside this container? The value can be logical, or a string \code{"parent"}/\code{"document"}, or a string of selector.}

}
\examples{
# There is no example
NULL

}
