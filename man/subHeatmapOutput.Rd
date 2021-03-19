\name{subHeatmapOutput}
\alias{subHeatmapOutput}
\title{
UI for the sub-heatmaps
}
\description{
UI for the sub-heatmaps
}
\usage{
subHeatmapOutput(heatmap_id, title = NULL,
    width = 400, height = 350, containment = FALSE, internal = FALSE)
}
\arguments{

  \item{heatmap_id}{ID of the plot.}
  \item{title}{Title of the sub-heatmap.}
  \item{width}{Width of the sub-heatmap.}
  \item{height}{Height of the sub-heatmap.}
  \item{containment}{Whether the resizing is restricted in a certain parent div? Value can be \code{TRUE}/\code{FALSE} or a JQuery selector.}
  \item{internal}{Internally used.}

}
\seealso{
\code{\link{originalHeatmapOutput}}.
}
\examples{
# See examples on the help page of originalHeatmapOutput()
}
