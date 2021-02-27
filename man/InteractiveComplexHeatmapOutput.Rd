\name{InteractiveComplexHeatmapOutput}
\alias{InteractiveComplexHeatmapOutput}
\title{
UI for the interactive ComplexHeatmap
}
\description{
UI for the interactive ComplexHeatmap
}
\usage{
InteractiveComplexHeatmapOutput(heatmap_id = NULL,
    title1 = "Original heatmap", title2 = "Selected sub-heatmap",
    width1 = ifelse(layout == "1|(2-3)", 800, 450),
    height1 = ifelse(layout == "1-(2|3)", 700, 350),
    width2 = 370,
    height2 = 350,
    width3 = ifelse(default_output_ui_float, 370, ifelse(layout == "(1-2)|3", 800, 370)),
    layout = "(1-2)|3",
    action = "click",
    brush_opt = list(stroke = "#f00", opacity = 0.6),
    output_ui = default_output_ui(),
    output_ui_float = FALSE,
    css = "", ...)
}
\arguments{

  \item{heatmap_id}{ID of the plot. If it is not specified, an internal ID is assigned.}
  \item{title1}{Title of the original heatmap.}
  \item{title2}{Title of the sub-heatmap.}
  \item{width1}{Width of the original heatmap.}
  \item{height1}{Height of the original heatmap.}
  \item{width2}{Width of the sub-heatmap.}
  \item{height2}{Height of the sub-heatmap.}
  \item{width3}{Width of the output div.}
  \item{layout}{One of \code{(1|2)-3}, \code{1-(2|3)}, \code{1-2-3}, \code{1|2|3}, \code{1|(2-3)}.}
  \item{action}{Which action for selecting single cell on the heatmap? Value should be \code{click}, \code{hover} or \code{dblclick}.}
  \item{brush_opt}{A list of parameters passed to \code{\link[shiny]{brushOpts}}. Do not set an ID for the brush. An internal brush ID is automatically set.}
  \item{output_ui}{A \code{\link[shiny]{htmlOutput}} or other output object. If it is set to \code{NULL}, there is no output in the app.}
  \item{output_ui_float}{Whether the UI defined by \code{output_ui} floats at the mouse positions.}
  \item{css}{Self-defined CSS code.}
  \item{...}{Pass to the UI container which is wrapped by \code{\link[shiny]{fluidPage}}.}

}
\details{
This function generates HTML fragment for the interactive UI. See the example from \code{\link{makeInteractiveComplexHeatmap}} page.

\code{layout} is defined as follows:

\itemize{
  \item \code{"(1-2)|3"}: Heatmap and sub-heatmap are in a same row, and output is in a second row. This is the default layout.
  \item \code{"1|(2-3)"}: Heatmap is in a single row, while sub-heatmap and output are in a second row.
  \item \code{"1-2-3"}: All three components are in a same row.
  \item \code{"1|2|3"}: Each component is in a single row.
  \item \code{"1-(2|3)"}: Being different from the other four layouts, this is a two-column layout. Heatmap is in a sigle column. Sub-heatmap and output are vertically aligned and the two are in the second column. 
}

The hover event is implemented with \url{https://github.com/websanova/mousestop} .
}
\value{
A UI that can be used in shiny.
}
\examples{
# There is no example
NULL

}
