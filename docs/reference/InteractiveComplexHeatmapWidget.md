# Interactive complex heatmap widget

Interactive complex heatmap widget

## Usage

``` r
InteractiveComplexHeatmapWidget(
  input,
  output,
  session,
  ht_list,
  heatmap_id = NULL,
  output_id,
  title1 = "Original heatmap",
  title2 = "Selected sub-heatmap",
  width1 = ifelse(layout == "1|(2-3)", 800, 450),
  height1 = ifelse(layout == "1-(2|3)", 700, 350),
  width2 = 370,
  height2 = 350,
  width3 = ifelse(layout == "(1-2)|3", 800, 370),
  layout = ifelse("brush" %in% response, "(1-2)|3", "1-3"),
  compact = FALSE,
  action = "click",
  cursor = TRUE,
  response = c(action, "brush"),
  brush_opt = list(stroke = "#f00", opacity = 0.6),
  output_ui = TRUE,
  output_ui_float = FALSE,
  click_action = NULL,
  brush_action = NULL,
  js_code = "",
  close_button = TRUE,
  cancel_action = c("remove", "hide")
)
```

## Arguments

- input:

  Passed from the Shiny server function.

- output:

  Passed from the Shiny server function.

- session:

  Passed from the Shiny server function.

- ht_list:

  A
  [`ComplexHeatmap::Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap-class.html)
  or a
  [`ComplexHeatmap::HeatmapList`](https://rdrr.io/pkg/ComplexHeatmap/man/HeatmapList-class.html)
  object.

- heatmap_id:

  ID of the plot. If it is not specified, an internal ID is assigned.

- output_id:

  Where the heatmap is put.

- title1:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- title2:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- width1:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- height1:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- width2:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- height2:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- width3:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- layout:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- compact:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- action:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- cursor:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- response:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- brush_opt:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- output_ui:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- output_ui_float:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- click_action:

  Pass to
  [`makeInteractiveComplexHeatmap()`](makeInteractiveComplexHeatmap.md).

- brush_action:

  Pass to
  [`makeInteractiveComplexHeatmap()`](makeInteractiveComplexHeatmap.md).

- js_code:

  Additional JavaScript code that is put after the interactive heatmap
  UI. The value can be a text or a function that takes "heatmap ID" as
  the argument and returns the formatted JavaScript code.

- close_button:

  Whether to add a close button at the end of the widget.

- cancel_action:

  Whether to remove the UI from HTML or just hide it when the UI is
  closed.

## Value

No value is returned.

## Details

It creates an interactive heatmap widget according to a certain action.
The UI is placed to the output ID that user defined.

The function is normally put inside
[`shiny::observe()`](https://rdrr.io/pkg/shiny/man/observe.html) or
[`shiny::observeEvent()`](https://rdrr.io/pkg/shiny/man/observeEvent.html).

## Examples

``` r
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
```
