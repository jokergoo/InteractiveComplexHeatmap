\name{HeatmapInfoOutput}
\alias{HeatmapInfoOutput}
\title{
UI for the output
}
\description{
UI for the output
}
\usage{
HeatmapInfoOutput(heatmap_id, title = NULL, width = 400,
    output_ui = default_output_ui(heatmap_id),
    output_ui_float = FALSE, action = NULL, response = NULL)
}
\arguments{

  \item{heatmap_id}{ID of the plot.}
  \item{title}{Title of the output.}
  \item{width}{Width of the output div.}
  \item{output_ui}{A \code{\link[shiny]{htmlOutput}} or other output object. If it is set to \code{NULL}, there is no output in the app. If it is not set, default response in server side on user's actions (i.e. click, hover or double click) is used, i.e. to print the value of the clicked/hovered cell when it is a click/hover/double click action or a runnable code to get the information of the sub-heatmap that was selected from heatmap when it is a brush action.}
  \item{output_ui_float}{Whether the UI defined by \code{output_ui} floats at the mouse positions.}
  \item{action}{It is only used when \code{output_ui_float} is set to \code{TRUE} to properly bind the floating frame to the event on heatmap. If \code{\link{HeatmapInfoOutput}} is executed after \code{\link{mainHeatmapOutput}}, the value for it is automatically assigned.}
  \item{response}{It is only used when \code{output_ui_float = TRUE} and \code{response = "brush"}, so that clicking or hovering in a same position won't have any effect, in other word, there is only response from brushing. If \code{\link{HeatmapInfoOutput}} is executed after \code{\link{mainHeatmapOutput}}, the value for it is automatically assigned.}

}
\examples{
# There is no example
NULL

}
