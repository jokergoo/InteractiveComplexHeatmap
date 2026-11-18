# Interactive heatmaps as a Shiny app

Interactive heatmaps as a Shiny app

## Usage

``` r
htShiny(
  ht_list = get_last_ht(),
  title = NULL,
  description = NULL,
  hline = TRUE,
  html = NULL,
  heatmap_id = NULL,
  title1 = "Original heatmap",
  title2 = "Selected sub-heatmap",
  width1 = ifelse(layout == "1|(2-3)", 800, 450),
  height1 = ifelse(layout == "1-(2|3)", 700, 350),
  width2 = 400,
  height2 = 350,
  width3 = ifelse(layout == "(1-2)|3", 800, 400),
  layout = ifelse("brush" %in% response, "(1-2)|3", "1-3"),
  compact = FALSE,
  action = "click",
  cursor = TRUE,
  response = c(action, "brush"),
  brush_opt = list(stroke = "#f00", opacity = 0.6),
  output_ui_float = FALSE,
  show_cell_fun = TRUE,
  show_layer_fun = TRUE,
  save = NULL,
  app_options = list()
)
```

## Arguments

- ht_list:

  A
  [`ComplexHeatmap::Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap-class.html)
  or a
  [`ComplexHeatmap::HeatmapList`](https://rdrr.io/pkg/ComplexHeatmap/man/HeatmapList-class.html)
  object. If it is not specified, the last generated heatmap is used.
  The heatmap object should better be already updated by
  [`ComplexHeatmap::draw()`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-dispatch.html)
  function.

- title:

  Title of the app.

- description:

  Description of the app. The content will be wrapped by a `<p>` tag and
  inserted before the interactive heatmap widget.

- hline:

  Whether to add the horizontal line (by `<hr>` tag) after
  `<description>`.

- html:

  HTML fragment inserted below the heatmap. The value can be a string or
  be wrapped by
  [`shiny::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html).

- heatmap_id:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

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

- output_ui_float:

  Pass to
  [`InteractiveComplexHeatmapOutput()`](InteractiveComplexHeatmapOutput.md).

- show_cell_fun:

  Whether show graphics made by `cell_fun` on the main heatmap?

- show_layer_fun:

  Whether show graphics made by `cell_fun` on the main heatmap?

- save:

  The value can be set to a folder name so that the shiny app is saved
  into several files.

- app_options:

  All pass to the `options` argument in
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

## Value

A Shiny app object.

## Details

With any `Heatmap`/`HeatmapList` object, directly send to `htShiny()` to
create a Shiny app for the heatmap(s):

    htShiny(ht_list)

If the heatmaps are already drawn, `ht_list` can be omitted and the last
heatmap object is retrieved automatically:

    Heatmap(...) + other_heatmaps_or_annotations # or other functions that internally use Heatmap()
    htShiny()

## See also

- https://jokergoo.shinyapps.io/interactive_complexheatmap/

- https://jokergoo.shinyapps.io/interactive_complexheatmap_vertical/

- https://jokergoo.shinyapps.io/interactive_densityheatmap/

- https://jokergoo.shinyapps.io/interactive_oncoprint/

- https://jokergoo.shinyapps.io/interactive_enrichedheatmap/

- https://jokergooo.shinyapps.io/interactive_upsetp/

- https://jokergooo.shinyapps.io/interactive_pheatmap/

- https://jokergooo.shinyapps.io/interactive_heatmap/

- https://jokergooo.shinyapps.io/interactive_heatmap_2/

- https://jokergooo.shinyapps.io/interactive_tidyheatmap/

There are also many examples that can be get with
[`htShinyExample()`](htShinyExample.md).

## Examples

``` r
# use last generated heatmap
if(interactive() && dev.interactive()) {
    m = matrix(rnorm(100), 10)
    Heatmap(m)
    htShiny()
}

# by providing a heatmap/heatmap list
if(interactive()) {
    m = matrix(rnorm(100), 10)
    rownames(m) = 1:10
    colnames(m) = 1:10
    
    ht = Heatmap(m)
    ht = draw(ht)
    htShiny(ht)
}

# vertical heatmap list
if(interactive()) {
    m1 = matrix(rnorm(100), 10)
    rownames(m1) = 1:10
    colnames(m1) = 1:10
    ht1 = Heatmap(m1, row_km = 2, column_km = 2)
    
    m2 = matrix(sample(letters[1:10], 100, replace = TRUE), 10)
    ht2 = Heatmap(m2)
    
    ht_list = draw(ht1 + ht2)
    htShiny(ht_list)
    
    ht_list = ht1 %v% ht2
    htShiny(ht_list)
}

# compact mode
if(interactive()) {
    m = matrix(rnorm(100), 10)
    Heatmap(m)
    htShiny(compact = TRUE)
}
```
