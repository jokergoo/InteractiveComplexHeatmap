# Get the position of hovered point on the heatmap image

Get the position of hovered point on the heatmap image

## Usage

``` r
getPositionFromHover(hover, ratio = 1)
```

## Arguments

- hover:

  The input hover object. Assume `heatmap_hover` is the ID set to
  argument `hover` in
  [`shiny::plotOutput()`](https://rdrr.io/pkg/shiny/man/plotOutput.html),
  then the value here is `input$heatmap_hover`.

- ratio:

  The relative resolution. The value should the ratio between `res` set
  in
  [`makeInteractiveComplexHeatmap()`](makeInteractiveComplexHeatmap.md)
  and 72 (`res/72`).

## Value

A [`grid::unit`](https://rdrr.io/r/grid/unit.html) object of length two
which are the coordinates of the hover points.
