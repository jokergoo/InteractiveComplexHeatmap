\name{InteractiveComplexHeatmapOutput}
\alias{InteractiveComplexHeatmapOutput}
\title{
UI for the interactive complex heatmaps
}
\description{
UI for the interactive complex heatmaps
}
\usage{
InteractiveComplexHeatmapOutput(heatmap_id = NULL,
    title1 = "Original heatmap", title2 = "Selected sub-heatmap",
    width1 = ifelse(layout == "1|(2-3)", 800, 450),
    height1 = ifelse(layout == "1-(2|3)", 700, 350),
    width2 = 370,
    height2 = 350,
    width3 = ifelse(default_output_ui_float, 370, ifelse(layout == "(1-2)|3", 800, 370)),
    layout = ifelse("brush" \%in\% response, "(1-2)|3", "1-3"),
    action = "click",
    response = c(action, "brush"),
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
  \item{layout}{One of \code{"(1|2)-3"}, \code{"1-(2|3)"}, \code{"1-2-3"}, \code{"1|2|3"}, \code{"1|(2-3)"}. If there is no response to \code{brush} which is set with the \code{response} argument, the code \code{2} can be omitted.}
  \item{action}{Which action for selecting single cell on the heatmap? Value should be \code{click}, \code{hover} or \code{dblclick}.}
  \item{response}{Which action needs to be respond on the server side. Value should be in \code{click}/\code{hover}/\code{dblclick} and \code{brush}. Please note, if \code{brush} is not selected, there is no "search tool" in the main heatmap.}
  \item{brush_opt}{A list of parameters passed to \code{\link[shiny]{brushOpts}}. Do not set an ID for the brush. An internal brush ID is automatically set.}
  \item{output_ui}{A \code{\link[shiny]{htmlOutput}} or other output object. If it is set to \code{NULL}, there is no output in the app. If it is not set, default response in server side on user's actions (i.e. click, hover or double click) is used, i.e. to print the value of the clicked/hovered cell when it is a click/hover/double click action or a runnable code to get the information of the sub-heatmap that was selected from heatmap when it is a brush action.}
  \item{output_ui_float}{Whether the UI defined by \code{output_ui} floats at the mouse positions.}
  \item{css}{Self-defined CSS code.}
  \item{...}{Pass to the UI container which is wrapped by \code{\link[shiny]{fluidPage}}.}

}
\details{
This function generates HTML fragment for the interactive UI. See the example in \code{\link{makeInteractiveComplexHeatmap}} page.

\code{layout} is defined as follows (\code{1} for the original heatmap, \code{2} for the selected sub-heatmap and \code{3} is for the output:

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
A UI that can be used in Shiny.
}
\examples{
# There is no example
NULL

}
