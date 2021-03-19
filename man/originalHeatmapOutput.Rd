\name{originalHeatmapOutput}
\alias{originalHeatmapOutput}
\title{
UI for the original heatmap
}
\description{
UI for the original heatmap
}
\usage{
originalHeatmapOutput(heatmap_id, title = NULL,
    width = 450, height = 350,
    action = "click", cursor = TRUE,
    response = c(action, "brush"),
    brush_opt = list(stroke = "#f00", opacity = 0.6),
    containment = FALSE, internal = FALSE)
}
\arguments{

  \item{heatmap_id}{ID of the plot.}
  \item{title}{Title of the original heatmap.}
  \item{width}{Width of the original heatmap.}
  \item{height}{Height of the original heatmap.}
  \item{action}{Which action for selecting single cells on the heatmap? Value should be \code{click}, \code{hover} or \code{dblclick}.}
  \item{cursor}{When moving mouse on heatmap, whether to show the cursors on the four sides?}
  \item{response}{Which action needs to be responded on the server side? Value should be in \code{click}/\code{hover}/\code{dblclick}, \code{brush} and \code{brush-output}. \code{brush} responds in two places which are the sub-heatmap and the output components and \code{brush-output} only responds in the output component.}
  \item{brush_opt}{A list of parameters passed to \code{\link[shiny]{brushOpts}}. Do not set an ID for the brush. An internal brush ID is automatically set.}
  \item{containment}{Whether the resizing is restricted in a certain parent div? Value can be \code{TRUE}/\code{FALSE} or a JQuery selector.}
  \item{internal}{Internally used.}

}
\seealso{
\code{\link{subHeatmapOutput}}, \code{\link{HeatmapInfoOutput}}.
}
\examples{
if(interactive()) {
    require(shinydashboard)
    m = matrix(rnorm(100), 10)
    ht = Heatmap(m)

    body = dashboardBody(
        fluidRow(
            box(title = "Original heatmap", width = 4, solidHeader = TRUE, status = "primary",
                originalHeatmapOutput("ht")
            ),
            box(title = "Sub-heatmap", width = 4, solidHeader = TRUE, status = "primary",
                subHeatmapOutput("ht")
            ),
            box(title = "Output", width = 4, solidHeader = TRUE, status = "primary",
                HeatmapInfoOutput("ht")
            )
        )
    )
    ui = dashboardPage(
        dashboardHeader(),
        dashboardSidebar(),
        body
    )
    server = function(input, output, session) {
        makeInteractiveComplexHeatmap(input, output, session, ht, "ht")
    }
    shinyApp(ui, server)
}
}
