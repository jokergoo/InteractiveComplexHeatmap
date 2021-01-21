# Make Interactive Complex Heatmaps

[![Build Status](https://travis-ci.org/jokergoo/InteractiveComplexHeatmap.svg)](https://travis-ci.org/jokergoo/InteractiveComplexHeatmap) 
[![bioc](http://www.bioconductor.org/shields/downloads/devel/InteractiveComplexHeatmap.svg)](https://bioconductor.org/packages/stats/bioc/InteractiveComplexHeatmap/) 
[![bioc](http://www.bioconductor.org/shields/years-in-bioc/InteractiveComplexHeatmap.svg)](http://bioconductor.org/packages/devel/bioc/html/InteractiveComplexHeatmap.html)

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

### Directly turn heatmaps interactive

With any `Heatmap`/`HeatmapList` object, directly send to `htShiny()` to create a Shiny app for your heatmap(s):

```r
htShiny(ht_list)
```

If the heatmaps are already drawn, `ht_list` can be omitted and the last heatmap object is retrieved automatically:

```r
Heatmap(...) + other_heatmaps_or_annotations # or other functions that internally use Heatmap()
htShiny()
```

### Shiny app development

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
    renderInteractiveComplexHeatmap(input, output, session, ht)
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
    renderInteractiveComplexHeatmap(input, output, session, ht1, "ht1")
    renderInteractiveComplexHeatmap(input, output, session, ht2, "ht2")
}

shiny::shinyApp(ui, server)
```

The heatmap widget can be dynamically loaded:

```r
ui = fluidPage(
    actionButton("show_heatmap", "Generate_heatmap"),
)

server = function(input, output, session) {
    m = matrix(rnorm(100), 10)
    ht = Heatmap(m)

    observeEvent(input$show_heatmap, {
        InteractiveComplexHeatmapModal(input, output, session, ht)
    })
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

There are also many other examples provided in the package.

```r
htShinyExample()
```

```
## There are following examples. Individual example can be run by e.g. htShinyExample(1).
## 
## ──────── Simple examples ───────────────────────────────────────────────────────── 
##   1. A single heatmap with minimal arguments.
##   2. A single heatmap from a character matrix.
##   3. A single heatmap with annotations on both rows and columns.
##   4. A single heatmap where rows and columns are split.
##   5. A list of two heatmaps.
## 
## ──────── On other plots and packages ───────────────────────────────────────────── 
##   6. A density heatmap.
##   7. An oncoPrint.
##   8. A UpSet plot.
##   9. An interactive heatmap from pheatmap().
##  10. An interactive heatmap from heatmap().
##  11. An interactive heatmap from heatmap.2().
##  12. A heatmap produced from tidyHeatmap package.
##  13. Genome-scale heatmap.
## 
## ──────── Enriched heatmaps ─────────────────────────────────────────────────────── 
##  14. An enriched heatmap.
##  15. A list of enriched heatmaps.
##  16. An enriched heatmap with discrete signals.
## 
## ──────── On public datasets ────────────────────────────────────────────────────── 
##  17. An example from Lewis et al 2019. GitHub repo:
##      https://github.com/kevinblighe/E-MTAB-6141
##  18. Visualize cell heterogeneity from single cell RNASeq. It is from
##      Supplementary S2 of the ComplexHeatmap paper.
##      https://github.com/jokergoo/supplementary
##  19. Correlations between methylation, expression and other genomic features.
##      It is from Supplementary S3 of the ComplexHeatmap paper.
##      https://github.com/jokergoo/supplementary
## 
## ──────── Shiny app development ─────────────────────────────────────────────────── 
##  20. A single Shiny app with two interactive heatmap widgets.
##  21. Self-define the output. The selected sub-matrix is shown as a text table.
##  22. Self-define the output. Additional annotations for the selected gene are
##      shown.
##  23. Visualize Gene Ontology similarities. A list of selected GO IDs as well as
##      their descriptions are shown in the output.
## 
## ──────── Dynamicly generate heatmap widget in Shiny app ────────────────────────── 
##  24. Dynamically generate the widget with InteractiveComplexHeatmapOutput() and
##      renderInteractiveComplexHeatmap().
##  25. Dynamically generate the widget with InteractiveComplexHeatmapModal(). The
##      modal is triggered by an action button.
##  26. Dynamically select interactive heatmaps. The modal is triggered by radio
##      buttons.
##  27. Dynamically generate the widget. A customized Javascript code is inserted
##      after the UI to change the default behavior of the action button.
##  28. The widget is generated by InteractiveComplexHeatmapWidget() where the UI
##      is directly put in the place defined by htmlOutput().
##  29. The widget is generated by InteractiveComplexHeatmapWidget() and a
##      customized Javascript code is inserted after the UI.
## 
## ──────── Interactive R markdown document ───────────────────────────────────────── 
##  30. Integrate in interactive R Markdown document.
##  31. Integrate in interactive R Markdown document where the heatmap widgets are
##      dynamically generated.
```

## License

MIT @ Zuguang Gu

