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
    width1 = 450, height1 = 350, width2 = 370, height2 = 350,
    width3 = ifelse(layout == "12|3", 800, 500),
    layout = c("12|3", "1|23"),
    action = c("click", "hover", "dblclick"),
    brush_opt = list(stroke = "#f00", opacity = 0.6),
    output_div = TRUE, css = "")
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
  \item{layout}{One of \code{12|3} or \code{1|23}. \code{12|3} means the div of original heatmap and sub-heatmap are in a same row.  \code{1|23} means the div of sub-heatmap and the output are in a same row.}
  \item{action}{Which action for selecting single cell on the heatmap? Value should be \code{click}, \code{hover} or \code{dblclick}.}
  \item{brush_opt}{A list of parameters passed to \code{\link[shiny]{brushOpts}}.}
  \item{output_div}{Whether to add the output \code{div}.}
  \item{css}{Self-defined CSS code.}

}
\details{
This function generates HTML fragment for the interactive UI. See the example from \code{\link{makeInteractiveComplexHeatmap}} page.
}
\value{
A UI that can be used in shiny.
}
\examples{
# There is no example
NULL

}
