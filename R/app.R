
# == title
# Interactive heatmaps with a shiny app
#
# == param
# -ht_list A `ComplexHeatmap::Heatmap-class` or a `ComplexHeatmap::HeatmapList-class` object. If it is not specified, a random heatmap is used.
# -... Pass to `InteractiveComplexHeatmapOutput`.
#
# == seealso
# https://jokergoo.shinyapps.io/interactive_complexHeatmap/
#
# == example
# # use a random heatmap
# if(interactive()) {
#     ht_shiny()
# }
#
# # by providing a heatmap/heatmap list
# if(interactive()) {
#     m = matrix(rnorm(100), 10)
#     rownames(m) = 1:10
#     colnames(m) = 1:10
#
#     ht = Heatmap(m)
#     ht_shiny(ht)
# }
#
# if(interactive()) {
#     m1 = matrix(rnorm(100), 10)
#     rownames(m1) = 1:10
#     colnames(m1) = 1:10
#     ht1 = Heatmap(m1, row_km = 2, column_km = 2)
#
#     m2 = matrix(sample(letters[1:10], 100, replace = TRUE), 10)
#     ht2 = Heatmap(m2)
#     ht_shiny(ht1 + ht2)
#     ht_shiny(ht1 \%v\% ht2)
# }
ht_shiny = function(ht_list, ...) {
	if(missing(ht_list)) {
		cat("No heatmap is provided, use random heatmap\n")
	    m1 = matrix(rnorm(100), 10)
	    colnames(m1) = rownames(m1) = paste0("a", 1:10)
	    ht1 = Heatmap(m1, row_km = 2, column_km = 2)

	    m2 = matrix(sample(letters[1:10], 100, replace = TRUE), 10)
	    colnames(m2) = rownames(m2) = paste0("b", 1:10)
	    ht2 = Heatmap(m2, heatmap_legend_param = list(at = sort(unique(as.vector(m2)))))
	    ht_list = draw(ht1 + ht2)
	} else {
		ht_list = ht_list
	}
	
	ui = fluidPage(
		titlePanel("ComplexHeatmap Shiny App"),

		p("You can click a position or select an area from the heatmap(s). The original heatmap and the selected sub-heatmap can be resized by dragging from the bottom right. If the heatmap is too huge or you resize the heatmap too frequently, the heatmap might not be correctly updated. You can just need to slightly resize the heatmap again and wait for several seconds."),
		hr(),

		InteractiveComplexHeatmapOutput(...)
	)

	server = function(input, output, session) {
		MakeInteractiveComplexHeatmap(ht_list, input, output, session)
	}

	shiny::shinyApp(ui, server)
}
