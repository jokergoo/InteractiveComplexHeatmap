# Interactive R markdown document

####################################################################
# title: Integrate in interactive R Markdown document.

rmarkdown::run(system.file("examples", "rmarkdown.Rmd", package = "InteractiveComplexHeatmap"))


####################################################################
# title: Integrate in interactive R Markdown document where the heatmap widgets are dynamically generated.

rmarkdown::run(system.file("examples", "rmarkdown-dynamic.Rmd", package = "InteractiveComplexHeatmap"))
