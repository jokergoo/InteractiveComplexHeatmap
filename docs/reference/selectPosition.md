# Select a position in the heatmap

Select a position in the heatmap

## Usage

``` r
selectPosition(
  ht_list = get_last_ht(),
  pos = NULL,
  mark = TRUE,
  verbose = TRUE,
  ht_pos = NULL,
  calibrate = TRUE
)
```

## Arguments

- ht_list:

  A
  [`ComplexHeatmap::HeatmapList`](https://rdrr.io/pkg/ComplexHeatmap/man/HeatmapList-class.html)
  object returned by
  [`ComplexHeatmap::draw,Heatmap-method`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-Heatmap-method.html)
  or
  [`ComplexHeatmap::draw,HeatmapList-method`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-HeatmapList-method.html).
  If it is omitted, it uses the last generated heatmap.

- pos:

  If the value is `NULL`, it can be selected by click on the heatmap (of
  cource, the heatmap should be on the interactive graphics device). If
  it is set, it must be a
  [`grid::unit`](https://rdrr.io/r/grid/unit.html) object with length
  two which corresponds to the x and y position of the point.

- mark:

  Whether to mark the selected position as a point.

- verbose:

  Whether to print messages.

- ht_pos:

  A value returned by [`htPositionsOnDevice()`](htPositionsOnDevice.md).

- calibrate:

  Internally used. Mainly works for Rstudio desktop IDE.

## Value

A
[`S4Vectors::DataFrame`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html)
object with row indices and column indices corresponding to the selected
position.

## Details

The regions can be selected interactively or selected manually by
setting `pos`.

## Examples

``` r
if(dev.interactive()) {
    m = matrix(rnorm(100), 10)
    rownames(m) = 1:10
    colnames(m) = 1:10
    
    ht = Heatmap(m)
    ht = draw(ht)
    selectPosition(ht)
}
```
