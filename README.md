# Make ComplexHeatmap interactive

[![Build Status](https://travis-ci.org/jokergoo/InteractiveComplexHeatmap.svg)](https://travis-ci.org/jokergoo/InteractiveComplexHeatmap) 


<img src="https://user-images.githubusercontent.com/449218/82199376-e5eec600-98fd-11ea-9fca-ad95d405dc20.gif"  width='700'/>

## Install

Currently, it is only available on GitHub:

```r
devtools::install_github("jokergoo/InteractiveComplexHeatmap")
```

## Usage

With any `Heatmap`/`HeatmapList` object, directly send to `ht_shiny()` to create a shiny app for your heatmap(s):

```r
ht_shiny(ht_list)
```

There are also two functions for Shiny app development:

- `InteractiveComplexHeatmapOutput()`: for the UI.
- `MakeInteractiveComplexHeatmap()`: for processing on the sever side.

```r
ht = Heatmap(m)
ht = draw(ht)

ui = fluidPage(
    InteractiveComplexHeatmapOutput()
)

server = function(input, output, session) {
    MakeInteractiveComplexHeatmap(ht, input, output, session)
}

shiny::shinyApp(ui, server)
```

You can also put multiple heatmaps in the same shiny app:

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
    MakeInteractiveComplexHeatmap(ht1, input, output, session, "ht1")
    MakeInteractiveComplexHeatmap(ht2, input, output, session, "ht2")
}

shiny::shinyApp(ui, server)
```
