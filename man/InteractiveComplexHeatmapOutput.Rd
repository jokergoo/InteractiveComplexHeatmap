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
    title3 = if(output_ui_float) NULL else "Output",
    width1 = ifelse(layout == "1|(2-3)", 800, 450),
    height1 = ifelse(layout == "1-(2|3)", 700, 350),
    width2 = 400,
    height2 = 350,
    width3 = NULL,
    layout = ifelse("brush" \%in\% response, "(1-2)|3", "1-3"), compact = FALSE,
    action = "click", cursor = TRUE,
    response = c(action, "brush"),
    brush_opt = list(stroke = "#f00", opacity = 0.6),
    output_ui = default_output_ui(heatmap_id),
    output_ui_float = FALSE, containment = FALSE,
    internal = FALSE,
    ...)
}
\arguments{

  \item{heatmap_id}{ID of the plot. If it is not specified, an internal ID is assigned.}
  \item{title1}{Title of the original heatmap.}
  \item{title2}{Title of the sub-heatmap.}
  \item{title3}{Title of the output.}
  \item{width1}{Width of the original heatmap.}
  \item{height1}{Height of the original heatmap.}
  \item{width2}{Width of the sub-heatmap.}
  \item{height2}{Height of the sub-heatmap.}
  \item{width3}{Width of the output div.}
  \item{layout}{One of \code{"(1|2)-3"}, \code{"1-(2|3)"}, \code{"1-2-3"}, \code{"1|2|3"}, \code{"1|(2-3)"}. If \code{brush} is not set with the argument \code{response}, which means there is no sub-heatmap panel, the code \code{2} can be omitted.}
  \item{compact}{If the value is \code{TRUE}, there will be no sub-heatmap, and output floats at the mouse position when click/hover on the original heatmap.}
  \item{action}{Which action for selecting single cells on the heatmap? Value should be \code{click}, \code{hover} or \code{dblclick}.}
  \item{cursor}{When moving mouse on heatmap, whether to show the cursors on the four sides?}
  \item{response}{Which action needs to be responded on the server side? Value should be in \code{click}/\code{hover}/\code{dblclick}, \code{brush} and \code{brush-output}. \code{brush} responds in two places which are the sub-heatmap and the output components and \code{brush-output} only responds in the output component.}
  \item{brush_opt}{A list of parameters passed to \code{\link[shiny]{brushOpts}}. Do not set an ID for the brush. An internal brush ID is automatically set.}
  \item{output_ui}{A \code{\link[shiny]{htmlOutput}} or other \code{*Output} object (defined in shiny or other related packages). If it is set to \code{NULL}, there is no output component in the app.}
  \item{output_ui_float}{Whether the UI defined by \code{output_ui} floats at the mouse positions.}
  \item{containment}{Whether the resizing is restricted in a certain parent div? Value can be \code{TRUE}/\code{FALSE} or a JQuery selector.}
  \item{internal}{Internally used.}
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
