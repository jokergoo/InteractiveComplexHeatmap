\name{InteractiveComplexHeatmapWidget}
\alias{InteractiveComplexHeatmapWidget}
\title{
Interactive complex heatmap widget
}
\description{
Interactive complex heatmap widget
}
\usage{
InteractiveComplexHeatmapWidget(
    input, output, session, get_heatmap, heatmap_id = NULL, output_id,
    
    # parameters passed to InteractiveComplexHeatmapOutput()
    title1 = "Original heatmap", title2 = "Selected sub-heatmap",
    width1 = ifelse(layout \%in\% c("1|(2+3)", "1|23"), 800, 450),
    height1 = ifelse(layout \%in\% c("1+(2|3)"), 700, 350),
    width2 = 370,
    height2 = 350,
    width3 = ifelse(layout \%in\% c("(1+2)|3", "12|3"), 800, 370),
    layout = "(1+2)|3",
    action = "click", brush_opt = list(stroke = "#f00", opacity = 0.6),
    output_ui = TRUE,
    
    # parameters passed to makeInteractiveComplexHeatmap()
    click_action = NULL, brush_action = NULL,
    
    # other configurations
    js_code = "", close_button = TRUE, cancel_action = c("remove", "hide"))
}
\arguments{

  \item{input}{Passed from the Shiny server function.}
  \item{output}{Passed from the Shiny server function.}
  \item{session}{Passed from the Shiny server function.}
  \item{get_heatmap}{A \code{\link[ComplexHeatmap]{Heatmap-class}} or a \code{\link[ComplexHeatmap]{HeatmapList-class}} object. The value can also be a function with no argument that generates such object.}
  \item{heatmap_id}{ID of the plot. If it is not specified, an internal ID is assigned.}
  \item{output_id}{Where the heatmap is put.}
  \item{title1}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{title2}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{width1}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{height1}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{width2}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{height2}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{width3}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{layout}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{action}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{brush_opt}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{output_ui}{Pass to \code{\link{InteractiveComplexHeatmapOutput}}.}
  \item{click_action}{Pass to \code{\link{makeInteractiveComplexHeatmap}}.}
  \item{brush_action}{Pass to \code{\link{makeInteractiveComplexHeatmap}}.}
  \item{js_code}{Additional JavaScript code that is put after the interactive heatmap UI. The value can be a text or a function that takes "heatmap ID" as the argument and returns the formatted JavaScript code.}
  \item{close_button}{Whether to add a close button at the end of the widget.}
  \item{cancel_action}{Whether to remove the UI from HTML or just hide it when the UI is closed.}

}
\details{
It create an interactive heatmap widget according to a certain action. The UI is placed to the output ID that user defined.
}
\value{
No value is returned.
}
\examples{
if(interactive()) {
    require(ComplexHeatmap)

    ui = fluidPage(
        actionButton("show_heatmap", "Generate_heatmap"),
        htmlOutput("heatmap_output")
    )

    server = function(input, output, session) {
        m = matrix(rnorm(100), 10)
        ht = Heatmap(m)

        observeEvent(input$show_heatmap, {
            InteractiveComplexHeatmapWidget(input, output, session, ht, 
                output_id = "heatmap_output")
        })
    }
    shiny::shinyApp(ui, server)
}
}
