# UI for the interactive complex heatmaps

UI for the interactive complex heatmaps

## Usage

``` r
InteractiveComplexHeatmapOutput(
  heatmap_id = NULL,
  title1 = "Original heatmap",
  title2 = "Selected sub-heatmap",
  title3 = if (output_ui_float) NULL else "Output",
  width1 = ifelse(layout == "1|(2-3)", 800, 450),
  height1 = ifelse(layout == "1-(2|3)", 700, 350),
  width2 = 400,
  height2 = 350,
  width3 = NULL,
  layout = ifelse("brush" %in% response, "(1-2)|3", "1-3"),
  compact = FALSE,
  action = "click",
  cursor = TRUE,
  response = c(action, "brush"),
  brush_opt = list(stroke = "#f00", opacity = 0.6),
  output_ui = default_output_ui(heatmap_id),
  output_ui_float = FALSE,
  containment = FALSE,
  internal = FALSE,
  ...
)
```

## Arguments

- heatmap_id:

  ID of the plot. If it is not specified, an internal ID is assigned.

- title1:

  Title of the original heatmap.

- title2:

  Title of the sub-heatmap.

- title3:

  Title of the output.

- width1:

  Width of the original heatmap.

- height1:

  Height of the original heatmap.

- width2:

  Width of the sub-heatmap.

- height2:

  Height of the sub-heatmap.

- width3:

  Width of the output div.

- layout:

  One of `"(1|2)-3"`, `"1-(2|3)"`, `"1-2-3"`, `"1|2|3"`, `"1|(2-3)"`. If
  `brush` is not set with the argument `response`, which means there is
  no sub-heatmap panel, the code `2` can be omitted.

- compact:

  If the value is `TRUE`, there will be no sub-heatmap, and output
  floats at the mouse position when click/hover on the original heatmap.

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

- output_ui:

  A
  [`shiny::htmlOutput()`](https://rdrr.io/pkg/shiny/man/htmlOutput.html)
  or other `*Output` object (defined in shiny or other related
  packages). If it is set to `NULL`, there is no output component in the
  app.

- output_ui_float:

  Whether the UI defined by `output_ui` floats at the mouse positions.

- containment:

  Whether the resizing is restricted in a certain parent div? Value can
  be `TRUE`/`FALSE` or a JQuery selector.

- internal:

  Internally used.

- ...:

  Pass to the UI container which is wrapped by
  [`shiny::fluidPage()`](https://rdrr.io/pkg/shiny/man/fluidPage.html).

## Value

A UI that can be used in Shiny.

## Details

This function generates HTML fragment for the interactive UI. See the
example in
[`makeInteractiveComplexHeatmap()`](makeInteractiveComplexHeatmap.md)
page.

`layout` is defined as follows (`1` for the original heatmap, `2` for
the selected sub-heatmap and `3` is for the output:

- `"(1-2)|3"`: Heatmap and sub-heatmap are in a same row, and output is
  in a second row. This is the default layout.

- `"1|(2-3)"`: Heatmap is in a single row, while sub-heatmap and output
  are in a second row.

- `"1-2-3"`: All three components are in a same row.

- `"1|2|3"`: Each component is in a single row.

- `"1-(2|3)"`: Being different from the other four layouts, this is a
  two-column layout. Heatmap is in a sigle column. Sub-heatmap and
  output are vertically aligned and the two are in the second column.

The hover event is implemented with
https://github.com/websanova/mousestop .
