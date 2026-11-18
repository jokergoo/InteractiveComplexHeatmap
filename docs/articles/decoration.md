# Decorations on heatmaps

    ## Warning: package 'knitr' was built under R version 4.6.1

In **ComplexHeatmap**, functions `decorate_*()` (*e.g.*
[`decorate_annotation()`](https://rdrr.io/pkg/ComplexHeatmap/man/decorate_annotation.html))
are used to add graphics after the heatmaps are drawn. To also put the
decoration graphics in heatmaps in the Shiny app, user needs to wrap all
the decoration code into a function and send it with `post_fun` argument
to [`draw()`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-dispatch.html)
function. For example, the following code generates a decorated heatmap:

``` r

library(ComplexHeatmap)
m = matrix(rnorm(100), 10)
ht = Heatmap(m, name = "foo")
ht = draw(ht)
decorate_heatmap_body("foo", {
    grid.text("some text", gp = gpar(fontsize = 30))
})
```

To export it as a Shiny app also with decorations, the code needs to be
slightly adjusted to:

``` r

library(InteractiveComplexHeatmap)
post_fun = function(ht_list) {
    decorate_heatmap_body("foo", {
        grid.text("some text", gp = gpar(fontsize = 30))
    })
}
ht = draw(ht, post_fun = post_fun)
htShiny(ht)
```

`post_fun` is a self-defined function and always needs one argument
`ht_list`. Since `post_fun` is applied after the heatmaps are drawn,
`ht_list` used in `post_fun` actually contains the clustering results
(if they are applied). Thus, you can do something like follows in
`post_fun`:

``` r

post_fun = function(ht_list) {
    decorate_heatmap_body("foo", {
        row_dend(ht_list)
        row_order(ht_list)
        ...
    })
}
```

Note the decorations will not be applied on the sub-heatmap, so you will
not see the decoration graphics there.

\
\
\
\
\
\
\
\
