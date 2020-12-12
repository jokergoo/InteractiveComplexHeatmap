\name{InteractiveComplexHeatmapOutput}
\alias{InteractiveComplexHeatmapOutput}
\title{
HTML code generated for interactive ComplexHeatmap
}
\description{
HTML code generated for interactive ComplexHeatmap
}
\usage{
InteractiveComplexHeatmapOutput(heatmap_id = NULL,
    width1 = 400, height1 = 350, width2 = 370, height2 = 350, nrow = 1,
    action = c("click", "hover", "dblclick"), brush_opt = list(), css = "")
}
\arguments{

  \item{heatmap_id}{ID of the plot. If it is not specified, an internal ID is assigned.}
  \item{width1}{Width of the original heatmap.}
  \item{height1}{Height of the original heatmap.}
  \item{width2}{Width of the sub-heatmap.}
  \item{height2}{Height of the sub-heatmap.}
  \item{nrow}{Should the two heatmap div put in one row or in two rows? Value should be either 1 or 2. }
  \item{action}{Which action for capturing. Value should be \code{click}, \code{hover} or \code{dblclick}.}
  \item{brush_opt}{A list of parameters passed to \code{\link[shiny]{brushOpts}}.}
  \item{css}{Self-defined CSS code.}

}
\details{
This function generates HTML fragment for the interactive UI. See the example from \code{\link{MakeInteractiveComplexHeatmap}} page.

It generates three div blocks. Assuming the heatmap id variable is \code{heatmap_id}, the three div blocks are:

\itemize{
  \item \code{#{heatmap_id}_heatmap_wrap_div}: to put the original heatmap. This div contains two children elements. One is the title for the heatmap (with a \code{h3} tag) and one is a div block with ID \code{#{heatmap_id}_heatmap_wrap}. \code{#{heatmap_id}_heatmap_wrap} is for JQuery-UI and it wraps the div \code{#{heatmap_id}_heatmap} which is used by \code{\link[shiny]{plotOutput}}.
  \item \code{#{heatmap_id}_sub_heatmap_wrap_div}: to put the sub-heatmap. This div contains two children elements. One is the title for the heatmap (with a \code{h3} tag) and one is a div block with ID \code{#{heatmap_id}_sub_heatmap_wrap}. \code{#{heatmap_id}_sub_heatmap_wrap} is for JQuery-UI and it wraps the div \code{#{heatmap_id}_sub_heatmap} which is used by \code{\link[shiny]{plotOutput}}.
  \item \code{#{heatmap_id}_info}: to put the information of the selected position/area.
}
}
\examples{
# There is no example
NULL

}
