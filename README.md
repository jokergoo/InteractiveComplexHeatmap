# Make Interactive Complex Heatmaps

[![Build Status](https://travis-ci.org/jokergoo/InteractiveComplexHeatmap.svg)](https://travis-ci.org/jokergoo/InteractiveComplexHeatmap) 


<img src="https://user-images.githubusercontent.com/449218/104457409-542d7a80-55aa-11eb-8cf6-34775e49535c.gif"  width='700' border="black" />

## Install

**InteractiveComplexHeatmap** is available on
[Bioconductor](https://bioconductor.org/packages/InteractiveComplexHeatmap/),
you can install it by:

```r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("InteractiveComplexHeatmap")
```

If you want the latest version, install it directly from GitHub:

```r
library(devtools)
install_github("jokergoo/InteractiveComplexHeatmap")
```

## Usage

With any `Heatmap`/`HeatmapList` object, directly send to `htShiny()` to create a Shiny app for your heatmap(s):

```r
htShiny(ht_list)
```

If the heatmaps are already drawn, `ht_list` can be omitted and the last heatmap object is retrieved automatically:

```r
Heatmap(...) + other_heatmaps_or_annotations # or other functions that internally use Heatmap()
htShiny()
```

There are also two functions for Shiny app development:

- `InteractiveComplexHeatmapOutput()`: for the UI on the client side.
- `renderInteractiveComplexHeatmap()`: for processing on the sever side.

```r
ht = Heatmap(m)
ht = draw(ht)

ui = fluidPage(
    InteractiveComplexHeatmapOutput()
)

server = function(input, output, session) {
    renderInteractiveComplexHeatmap(ht, input, output, session)
}

shiny::shinyApp(ui, server)
```

You can also put multiple interactive heatmaps widgets in the same Shiny app:

```r
ht1 = Heatmap(m, col = c("white", "blue"))
ht1 = draw(ht1)
ht2 = Heatmap(m, col = c("white", "red"))
ht2 = draw(ht2)

ui = fluidPage(
    h3("The first heatmap"),
    InteractiveComplexHeatmapOutput("ht1"),
    hr(),
    h3("The second heatmap"),
    InteractiveComplexHeatmapOutput("ht2")
)

server = function(input, output, session) {
    renderInteractiveComplexHeatmap(ht1, input, output, session, "ht1")
    renderInteractiveComplexHeatmap(ht2, input, output, session, "ht2")
}

shiny::shinyApp(ui, server)
```

## Live examples

Following lists several live examples of interactive heatmaps. Details
can be found in the package vignette.

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

## License

MIT @ Zuguang Gu

