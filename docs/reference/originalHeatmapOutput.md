# UI for the original heatmap

UI for the original heatmap

## Usage

``` r
originalHeatmapOutput(
  heatmap_id,
  title = NULL,
  width = 450,
  height = 350,
  action = "click",
  cursor = TRUE,
  response = c(action, "brush"),
  brush_opt = list(stroke = "#f00", opacity = 0.6),
  containment = FALSE,
  internal = FALSE
)
```

## Arguments

- heatmap_id:

  ID of the plot.

- title:

  Title of the original heatmap.

- width:

  Width of the original heatmap.

- height:

  Height of the original heatmap.

- action:

  Which action for selecting single cells on the heatmap? Value should
  be `click`, `hover` or `dblclick`.

- cursor:

  When moving mouse on heatmap, whether to show the cursors on the four
  sides?

- response:

  Which action needs to be responded on the server side? Value should be
  in `click`/`hover`/`dblclick`, `brush` and `brush-output`. `brush`
  responds in two places which are the sub-heatmap and the output
  components and `brush-output` only responds in the output component.

- brush_opt:

  A list of parameters passed to
  [`shiny::brushOpts()`](https://rdrr.io/pkg/shiny/man/brushOpts.html).
  Do not set an ID for the brush. An internal brush ID is automatically
  set.

- containment:

  Whether the resizing is restricted in a certain parent div? Value can
  be `TRUE`/`FALSE` or a JQuery selector.

- internal:

  Internally used.

## See also

[`subHeatmapOutput()`](subHeatmapOutput.md),
[`HeatmapInfoOutput()`](HeatmapInfoOutput.md).

## Examples

``` r
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
```
