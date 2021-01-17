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

There are also many other examples provided in the package.

```r
htShinyExample()

## There are following examples:
## 
##  1. A single heatmap with minimal arguments.
##  2. A single heatmap from a character matrix.
##  3. A single heatmap with annotations on both rows and columns.
##  4. A single heatmap where rows and columns are split.
##  5. A list of two heatmaps.
##  6. A density heatmap.
##  7. An oncoPrint.
##  8. A UpSet plot.
##  9. An interactive heatmap by `pheatmap()`.
## 10. An interactive heatmap by `heatmap()`.
## 11. An interactive heatmap by `heatmap.2()`.
## 12. An enriched heatmap.
## 13. A list of enriched heatmaps.
## 14. An enriched heatmap with discrete signals.
## 15. A heatmap produced from tidyHeatmap package.
## 16. An example from Lewis et al 2019. GitHub repo:
##     https://github.com/kevinblighe/E-MTAB-6141
## 17. Visualize cell heterogeneity from single cell RNASeq. This is from
##     Supplementary S2 of the ComplexHeatmap paper.
##     https://github.com/jokergoo/supplementary/tree/master/ComplexHeatmap-supplementary1-4
## 18. Correlations between methylation, expression and other genomic features.
##     This is from Supplementary S3 of the ComplexHeatmap paper.
##     https://github.com/jokergoo/supplementary/tree/master/ComplexHeatmap-supplementary1-4
## 19. A single shiny app with two interactive heatmap widgets.
## 20. Self-define the output. The selected sub-matrix is shown as a text table.
## 21. Self-define the output. Additional annotations for the selected gene are
##     shown.
## 22. Integrate in an interactive R Markdown document.
## 23. Visualize Gene Ontology similarities. A list of selected GO IDs as well as
##     their descriptions are shown in the output.
## 24. Genome-scale heatmaps.
```

## License

MIT @ Zuguang Gu

