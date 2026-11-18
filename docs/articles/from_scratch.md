# Implement interactive heatmap from scratch

In **InteractiveComplexHeatmap**, we mainly use the combination of
[`InteractiveComplexHeatmapOutput()`](../reference/InteractiveComplexHeatmapOutput.md)
and
[`makeInteractiveComplexHeatmap()`](../reference/makeInteractiveComplexHeatmap.md)
to create the interactive heatmap widget in the Shiny application. The
interactive widgets contains many tools for manipulating heatmaps.
However, sometimes users may want to build their own interactive heatmap
widget, e.g. to define their own logic to respond to the clicking or
brushing event on heatmaps, while not use the one provided by
**InteractiveComplexHeatmap**. What they only want is the information of
the heatmap cells that were selected from heatmaps.

**InteractiveComplexHeatmap** also provides low-level functions that
directly return the information of rows and columns that were selected
from heatmap. I demonstrate the usage in the next example where `ui` and
`server` are defined as follows:

``` r

ui = fluidPage(
    ...,
    plotOutput("heatmap", click = "heatmap_click")
)
server = function(input, output, session) {
    ht_obj = reactiveVal(NULL)
    ht_pos_obj = reactiveVal(NULL)
    output$heatmap = renderPlot({
        ...
        ht = draw(Heatmap(mat))
        ht_pos = htPositionsOnDevice(ht)
        ht_obj(ht)
        ht_pos_obj(ht_pos)
    })
    observeEvent(input$heatmap_click, {
        pos = getPositionFromClick(input$heatmap_click)
        df = selectPosition(ht_obj(), pos, mark = FALSE, 
            ht_pos = ht_pos_obj(), verbose = FALSE)
        # do something with `df`
        ...
    })
}
```

There are two points that need to be noted.

1.  [`draw()`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-dispatch.html)
    and [`htPositionsOnDevice()`](../reference/htPositionsOnDevice.md)
    should always be executed together and be put inside
    [`renderPlot()`](https://rdrr.io/pkg/shiny/man/renderPlot.html) so
    that positions of all heatmap slices can be correctly calculated.
2.  Use [`getPositionFromClick()`](../reference/getPositionFromClick.md)
    to retrieve the position of the click on heatmap, then the position
    can be sent to [`selectPosition()`](../reference/selectPosition.md)
    to correspond to the original matrix. Similarly,
    [`getPositionFromBrush()`](../reference/getPositionFromBrush.md) and
    [`selectArea()`](../reference/selectArea.md) work together to
    retrieve rows and columns of the matrix that correspond to the area
    selected from heatmap.

The variable `df` contains row indices and column indices for the cells
that were selected (check the help page of
[`selectPosition()`](../reference/selectPosition.md) and
[`selectArea()`](../reference/selectArea.md) to see the format of `df`).
With this information, it is possible to implement the interactive
heatmap widget from scratch.

`htShinyExample(5.8)` demonstrates an example where the interactivity of
the heatmap is only implemented with the functions from **shiny**. In
this example, the main heatmap visualizes a 2D density distribution, and
brushing on heatmap triggers a new 2D density estimation but only on the
selected subset of data. The source code that generates this example is
also includes in this web application.

![](https://user-images.githubusercontent.com/449218/113612404-1479aa80-9650-11eb-8647-0f06cd1b7779.gif)
