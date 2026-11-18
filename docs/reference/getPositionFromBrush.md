# Get the position of the brushed area on the heatmap image

Get the position of the brushed area on the heatmap image

## Usage

``` r
getPositionFromBrush(brush, ratio = 1)
```

## Arguments

- brush:

  The input brush object. Assume `heatmap_brush` is the ID set to
  argument `brush` in
  [`shiny::plotOutput()`](https://rdrr.io/pkg/shiny/man/plotOutput.html),
  then the value here is `input$heatmap_brush`.

- ratio:

  The relative resolution. The value should the ratio between `res` set
  in
  [`makeInteractiveComplexHeatmap()`](makeInteractiveComplexHeatmap.md)
  and 72 (`res/72`).

## Value

A list of length two. The two elements correspons to the coordinates of
the two diagonal points.

## See also

[`getPositionFromClick()`](getPositionFromClick.md),
[`getPositionFromHover()`](getPositionFromHover.md),
[`getPositionFromDblclick()`](getPositionFromDblclick.md).
