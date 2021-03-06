# Make Interactive Complex Heatmaps

[![Build Status](https://travis-ci.org/jokergoo/InteractiveComplexHeatmap.svg)](https://travis-ci.org/jokergoo/InteractiveComplexHeatmap) 
[![bioc](http://www.bioconductor.org/shields/downloads/devel/InteractiveComplexHeatmap.svg)](https://bioconductor.org/packages/stats/bioc/InteractiveComplexHeatmap/) 
[![bioc](http://www.bioconductor.org/shields/years-in-bioc/InteractiveComplexHeatmap.svg)](http://bioconductor.org/packages/devel/bioc/html/InteractiveComplexHeatmap.html)

**InteractiveComplexHeatmap** is an R package that converts static heatmaps produced from
[**ComplexHeatmap**](https://github.com/jokergoo/ComplexHeatmap) package into an interactive
Shiny app only with one extra line of code.

<img src="https://user-images.githubusercontent.com/449218/110212910-e6147e00-7e9d-11eb-94ed-0ac549247888.gif"  width='100%' border="black" />

## Install

**InteractiveComplexHeatmap** is available on
[Bioconductor](https://bioconductor.org/packages/InteractiveComplexHeatmap/),
you can install it by:

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
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
- `makeInteractiveComplexHeatmap()`: for processing on the sever side.

```r
library(InteractiveComplexHeatmap)
library(ComplexHeatmap)

ht = Heatmap(m)
ht = draw(ht)

ui = fluidPage(
    InteractiveComplexHeatmapOutput()
)

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht)
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
    makeInteractiveComplexHeatmap(input, output, session, ht1, "ht1")
    makeInteractiveComplexHeatmap(input, output, session, ht2, "ht2")
}

shiny::shinyApp(ui, server)
```

Two additional functions to let you dynamically load interactive heatmap widgets:

- `InteractiveComplexHeatmapModal()`: The interactive heatmap widget is inserted as a "modal".
- `InteractiveComplexHeatmapWidget()`: The interactive heatmap widget is inserted into a place defined by users.

```r
m = matrix(rnorm(100), 10)
ht = Heatmap(m)
    
ui = fluidPage(
    actionButton("show_heatmap", "Generate_heatmap"),
)

server = function(input, output, session) {
    observeEvent(input$show_heatmap, {
        InteractiveComplexHeatmapModal(input, output, session, ht)
    })
}
shiny::shinyApp(ui, server)

# or use InteractiveComplexHeatmapWidget()
ui = fluidPage(
    actionButton("show_heatmap", "Generate_heatmap"),
    htmlOutput("heatmap_output")
)

server = function(input, output, session) {
    observeEvent(input$show_heatmap, {
        InteractiveComplexHeatmapWidget(input, output, session, ht,
            output_id = "heatmap_output")
    })
}
shiny::shinyApp(ui, server)
```

## Interactivate pheatmap(), heatmap.2() and heatmap()

If you directly use these three funtions, simply replace them with
`ComplexHeatmap::pheatmap()`, `ComplexHeatmap:::heatmap.2()` and
`ComplexHeatmap:::heatmap()`. If the three functions are used indirectly, e.g.
a function `foo()` (maybe from another packages or other people's functions)
which internally uses these three heatmap functions, check the vignette
["Interactivate indirect use of pheatmap(), heatmap.2() and heatmap()"](https://jokergoo.github.io/InteractiveComplexHeatmap/articles/interactivate_indirect.html) to find out how.

## Live examples

Following lists several live examples of interactive heatmaps. Details
can be found in the package vignette.

- https://jokergoo.shinyapps.io/interactive_complexheatmap/
- https://jokergoo.shinyapps.io/interactive_complexheatmap_vertical/
- https://jokergoo.shinyapps.io/interactive_densityheatmap/
- https://jokergoo.shinyapps.io/interactive_oncoprint/
- https://jokergoo.shinyapps.io/interactive_enrichedheatmap/
- https://jokergooo.shinyapps.io/interactive_upset/
- https://jokergooo.shinyapps.io/interactive_pheatmap/
- https://jokergooo.shinyapps.io/interactive_heatmap/
- https://jokergooo.shinyapps.io/interactive_heatmap_2/
- https://jokergooo.shinyapps.io/interactive_tidyheatmap/

There are also many other examples provided in the package.

```r
htShinyExample()
```

```
## There are following examples. Individual example can be run by e.g. htShinyExample(1.1).
## 
## ──────── 1. Simple examples ─────────────────────────────────────────────────────────
##  1.1 A single heatmap with minimal arguments.
##  1.2 A single heatmap from a character matrix.
##  1.3 A single heatmap with annotations on both rows and columns.
##  1.4 A single heatmap where rows and columns are split.
##  1.5 A list of two heatmaps.
##  1.6 Use last generated heatmap, an example from cola package.
##  1.7 Use last generated heatmap, an app with three interactive heatmaps
##  1.8 Demonstrate hover, click and dblclick actions.
##  1.9 Only response to one of click/hover/dblclick/hover events.
## 
## ──────── 2. On other plots and packages ─────────────────────────────────────────────
##  2.1 A density heatmap.
##  2.2 An oncoPrint.
##  2.3 A UpSet plot.
##  2.4 An interactive heatmap from pheatmap().
##  2.5 An interactive heatmap from heatmap().
##  2.6 An interactive heatmap from heatmap.2().
##  2.7 A heatmap produced from tidyHeatmap package.
##  2.8 Genome-scale heatmap.
## 
## ──────── 3. Enriched heatmaps ───────────────────────────────────────────────────────
##  3.1 An enriched heatmap.
##  3.2 A list of enriched heatmaps.
##  3.3 An enriched heatmap with discrete signals.
## 
## ──────── 4. On public datasets ──────────────────────────────────────────────────────
##  4.1 An example from Lewis et al 2019. GitHub repo:
##      https://github.com/kevinblighe/E-MTAB-6141
##  4.2 Visualize cell heterogeneity from single cell RNASeq. It is from
##      Supplementary S2 of the ComplexHeatmap paper.
##      https://github.com/jokergoo/supplementary
##  4.3 Correlations between methylation, expression and other genomic features.
##      It is from Supplementary S3 of the ComplexHeatmap paper.
##      https://github.com/jokergoo/supplementary
## 
## ──────── 5. Shiny app development ───────────────────────────────────────────────────
##  5.1 A single Shiny app with two interactive heatmap widgets.
##  5.2 Self-define the output. The selected sub-matrix is shown as a text table.
##  5.3 Self-define the output. Additional annotations for the selected gene are
##      shown.
##  5.4 Visualize Gene Ontology similarities. A list of selected GO IDs as well as
##      their descriptions are shown in the output.
##  5.5 Visualize a DESeq2 results. The selected genes are highlighted in an
##      associated MA plot.
##  5.6 Interactive correlation heatmap. Clicking on the cell generates a
##      scatterplot of the two corresponding variables.
## 
## ──────── 6. Dynamically generate heatmap widget in Shiny app ────────────────────────
##  6.1 The matrix is dynamically generated.
##  6.2 Reorder by a column that is specified by user.
##  6.3 Dynamically generate the widget with InteractiveComplexHeatmapModal(). The
##      modal is triggered by an action button.
##  6.4 Dynamically select interactive heatmaps. The modal is triggered by radio
##      buttons.
##  6.5 Dynamically generate the widget. A customized Javascript code is inserted
##      after the UI to change the default behavior of the action button.
##  6.6 The widget is generated by InteractiveComplexHeatmapWidget() where the UI
##      is directly put in the place defined by htmlOutput().
##  6.7 The widget is generated by InteractiveComplexHeatmapWidget() and a
##      customized Javascript code is inserted after the UI.
## 
## ──────── 7. Interactive R markdown document ─────────────────────────────────────────
##  7.1 Integrate in interactive R Markdown document.
##  7.2 Integrate in interactive R Markdown document where the heatmap widgets are
##      dynamically generated.
## 
## ──────── 8. Interactivate indirect use of pheatmap(), heatmap.2() and heatmap() ─────
##  8.1 Indirect use of pheatmap().
##  8.2 Indirect use of heatmap.2().
##  8.3 Two interactive heatmap widgets from indirect use of pheatmap().
## 
## ──────── 9. Float output UI along with mouse positions ──────────────────────────────
##  9.1 A simple example that demonstrates output UI floating with the three
##      actions: hover, click and dblclick.
##  9.2 floating self-defined outputs.
##  9.3 Floating output only from one event.
## 
```

## License

MIT @ Zuguang Gu

