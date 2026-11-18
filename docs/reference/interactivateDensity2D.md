# Interactive Shiny application for 2D density distribution

Interactive Shiny application for 2D density distribution

## Usage

``` r
interactivateDensity2D(x, y, ...)
```

## Arguments

- x:

  A numeric vector.

- y:

  A numeric vector.

- ...:

  All pass to [`ks::kde()`](https://mvstat.net/ks/reference/kde.html).

## Examples

``` r
if(interactive()) {
    lt = readRDS(system.file("extdata", "2d_density_xy.rds", package = "InteractiveComplexHeatmap"))
    interactivateDensity2D(lt$x, lt$y)
}
```
