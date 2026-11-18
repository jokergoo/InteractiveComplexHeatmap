# UI for the output

UI for the output

## Usage

``` r
HeatmapInfoOutput(
  heatmap_id,
  title = NULL,
  width = 400,
  output_ui = default_output_ui(heatmap_id),
  output_ui_float = FALSE,
  action = NULL,
  response = NULL,
  internal = FALSE
)
```

## Arguments

- heatmap_id:

  ID of the plot.

- title:

  Title of the output.

- width:

  Width of the output div.

- output_ui:

  A
  [`shiny::htmlOutput()`](https://rdrr.io/pkg/shiny/man/htmlOutput.html)
  or other `*Output` object (defined in shiny or other related
  packages).

- output_ui_float:

  Whether the UI defined by `output_ui` floats at the mouse positions.

- action:

  It is only used when `output_ui_float = TRUE` to properly bind the
  floating frame to the event on heatmap (i.e. `click`, `hover` or
  `dblclick`). If `HeatmapInfoOutput()` is executed after
  [`originalHeatmapOutput()`](originalHeatmapOutput.md), the value for
  it is automatically decided.

- response:

  It is only used when `output_ui_float = TRUE` and `response = "brush"`
  or `response = "brush-output"`, so that single clicking or hovering
  won't have any effect, in other word, there is only response from
  brushing. If `HeatmapInfoOutput()` is executed after
  [`originalHeatmapOutput()`](originalHeatmapOutput.md), the value for
  it is automatically decided.

- internal:

  Internally used.

## See also

[`originalHeatmapOutput()`](originalHeatmapOutput.md),
[`subHeatmapOutput()`](subHeatmapOutput.md).

## Examples

``` r
# See examples on the help page of originalHeatmapOutput()
```
