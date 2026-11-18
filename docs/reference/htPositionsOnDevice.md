# Get heatmap positions on the graphics device

Get heatmap positions on the graphics device

## Usage

``` r
htPositionsOnDevice(
  ht_list = get_last_ht(),
  unit = "inch",
  valueOnly = FALSE,
  include_annotation = FALSE,
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

- unit:

  The unit.

- valueOnly:

  Whether only return the numeric values.

- include_annotation:

  Internally used.

- calibrate:

  Internally used.

## Value

It returns a
[`S4Vectors::DataFrame`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html)
object of the position of every heatmap slice.

## Details

`ht_list` must have been already updated by
[`ComplexHeatmap::draw()`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-dispatch.html)
function. The function needs to be executed under a graphics device
where the heatmap is written.

## Examples

``` r
if(dev.interactive()) {
    m = matrix(rnorm(100), 10)
    ht = Heatmap(m, row_km = 2, column_km = 2)
    ht = draw(ht)
    pos = htPositionsOnDevice(ht)
    
    InteractiveComplexHeatmap:::redraw_ht_vp(pos)
}
```
