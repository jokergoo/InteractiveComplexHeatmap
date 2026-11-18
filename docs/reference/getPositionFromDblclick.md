# Get the position of double clicked point on the heatmap image

Get the position of double clicked point on the heatmap image

## Usage

``` r
getPositionFromDblclick(dblclick, ratio = 1)
```

## Arguments

- dblclick:

  The input dblclick object. Assume `heatmap_dblclick` is the ID set to
  argument `dblclick` in
  [`shiny::plotOutput()`](https://rdrr.io/pkg/shiny/man/plotOutput.html),
  then the value here is `input$heatmap_dblclick`.

- ratio:

  The relative resolution. The value should the ratio between `res` set
  in
  [`makeInteractiveComplexHeatmap()`](makeInteractiveComplexHeatmap.md)
  and 72 (`res/72`).

## Value

A [`grid::unit`](https://rdrr.io/r/grid/unit.html) object of length two
which are the coordinates of the double clicked points.
