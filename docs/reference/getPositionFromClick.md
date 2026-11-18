# Get the position of clicked point on the heatmap image

Get the position of clicked point on the heatmap image

## Usage

``` r
getPositionFromClick(click, ratio = 1)
```

## Arguments

- click:

  The input click object. Assume `heatmap_click` is the ID set to
  argument `click` in
  [`shiny::plotOutput()`](https://rdrr.io/pkg/shiny/man/plotOutput.html),
  then the value here is `input$heatmap_click`.

- ratio:

  The relative resolution. The value should the ratio between `res` set
  in `makeInteractiveComplexHeatmap` and 72 (`res/72`).

## Value

A [`grid::unit`](https://rdrr.io/r/grid/unit.html) object of length two
which are the coordinates of the clicked points.

## See also

[`getPositionFromBrush()`](getPositionFromBrush.md),
[`getPositionFromHover()`](getPositionFromHover.md),
[`getPositionFromDblclick()`](getPositionFromDblclick.md).
