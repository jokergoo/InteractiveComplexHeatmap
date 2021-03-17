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
  \item{output_ui}{A \code{\link[shiny]{htmlOutput}} or other \code{*Output} object (defined in shiny).}
  \item{output_ui_float}{Whether the UI defined by \code{output_ui} floats at the mouse positions.}
  \item{action}{It is only used when \code{output_ui_float = TRUE} to properly bind the floating frame to the event on heatmap (i.e. \code{click}, \code{hover} or \code{dblclick}). If \code{\link{HeatmapInfoOutput}} is executed after \code{\link{mainHeatmapOutput}}, the value for it is automatically assigned.}
  \item{response}{It is only used when \code{output_ui_float = TRUE} and \code{response = "brush"} or \code{response = "brush-output"}, so that single clicking or hovering won't have any effect, in other word, there is only response from brushing. If \code{\link{HeatmapInfoOutput}} is executed after \code{\link{mainHeatmapOutput}}, the value for it is automatically assigned.}

}
\seealso{
\code{\link{mainHeatmapOutput}}, \code{\link{subHeatmapOutput}}.
}
\examples{
# See examples on the help page of mainHeatmapOutput()
}
