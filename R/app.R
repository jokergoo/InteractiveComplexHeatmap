
# == title
# Interactive heatmaps as a Shiny app
#
# == param
# -ht_list A `ComplexHeatmap::Heatmap-class` or a `ComplexHeatmap::HeatmapList-class` object. If it is not specified, the last generated heatmap is used.
#     The heatmap object should better be already updated by ``draw()`` function.
# -title Title of the app.
# -description Description of the app. The content will be wrapped by a ``p`` tag and inserted before the interactive heatmap widget.
# -hline Whether to add the horizontal line (by ``hr`` tag) after ``description``.
# -html HTML fragment inserted below the heatmap. The value can be a string or be wrapped by `shiny::HTML`.
# -heatmap_id Pass to `InteractiveComplexHeatmapOutput`.
# -title1 Pass to `InteractiveComplexHeatmapOutput`.
# -title2 Pass to `InteractiveComplexHeatmapOutput`.
# -width1 Pass to `InteractiveComplexHeatmapOutput`.
# -height1 Pass to `InteractiveComplexHeatmapOutput`.
# -width2 Pass to `InteractiveComplexHeatmapOutput`.
# -height2 Pass to `InteractiveComplexHeatmapOutput`.
# -width3 Pass to `InteractiveComplexHeatmapOutput`.
# -layout Pass to `InteractiveComplexHeatmapOutput`.
# -compact Pass to `InteractiveComplexHeatmapOutput`.
# -action Pass to `InteractiveComplexHeatmapOutput`.
# -cursor Pass to `InteractiveComplexHeatmapOutput`.
# -response Pass to `InteractiveComplexHeatmapOutput`.
# -brush_opt Pass to `InteractiveComplexHeatmapOutput`.
# -output_ui_float Pass to `InteractiveComplexHeatmapOutput`.
# -sub_heatmap_cell_fun The ``cell_fun`` specifically defined for sub-heatmap.
# -sub_heatmap_layer_fun The ``layer_fun`` specifically defined for sub-heatmap.
# -save The value can be set to a folder name so that the shiny app is saved into several files.
#
# == details
# With any ``Heatmap``/``HeatmapList`` object, directly send to ``htShiny()`` to create a Shiny app for the heatmap(s):
#
#     htShiny(ht_list)
#
# If the heatmaps are already drawn, ``ht_list`` can be omitted and the last heatmap object is retrieved automatically:
#
#     Heatmap(...) + other_heatmaps_or_annotations # or other functions that internally use Heatmap()
#     htShiny()
#
# == seealso
# - https://jokergoo.shinyapps.io/interactive_complexheatmap/
# - https://jokergoo.shinyapps.io/interactive_complexheatmap_vertical/
# - https://jokergoo.shinyapps.io/interactive_densityheatmap/
# - https://jokergoo.shinyapps.io/interactive_oncoprint/
# - https://jokergoo.shinyapps.io/interactive_enrichedheatmap/
# - https://jokergooo.shinyapps.io/interactive_upsetp/
# - https://jokergooo.shinyapps.io/interactive_pheatmap/
# - https://jokergooo.shinyapps.io/interactive_heatmap/
# - https://jokergooo.shinyapps.io/interactive_heatmap_2/
# - https://jokergooo.shinyapps.io/interactive_tidyheatmap/
#
# There are also many examples that can be get with `htShinyExample`.
#
# == value
# A Shiny app object.
#
# == example
# # use last generated heatmap
# if(interactive() && dev.interactive()) {
#     m = matrix(rnorm(100), 10)
#     Heatmap(m)
#     htShiny()
# }
#
# # by providing a heatmap/heatmap list
# if(interactive()) {
#     m = matrix(rnorm(100), 10)
#     rownames(m) = 1:10
#     colnames(m) = 1:10
#     
#     ht = Heatmap(m)
#     ht = draw(ht)
#     htShiny(ht)
# }
#
# # vertical heatmap list
# if(interactive()) {
#     m1 = matrix(rnorm(100), 10)
#     rownames(m1) = 1:10
#     colnames(m1) = 1:10
#     ht1 = Heatmap(m1, row_km = 2, column_km = 2)
#     
#     m2 = matrix(sample(letters[1:10], 100, replace = TRUE), 10)
#     ht2 = Heatmap(m2)
#     
#     ht_list = draw(ht1 + ht2)
#     htShiny(ht_list)
#     
#     ht_list = ht1 \%v\% ht2
#     htShiny(ht_list)
# }
#
# # compact mode
# if(interactive()) {
#     m = matrix(rnorm(100), 10)
#     Heatmap(m)
#     htShiny(compact = TRUE)
# }
htShiny = function(ht_list = get_last_ht(), title = NULL, 
	description = NULL, hline = TRUE, html = NULL, 

	# parameters passed to InteractiveComplexHeatmapOutput()
	heatmap_id = NULL, title1 = "Original heatmap", title2 = "Selected sub-heatmap",
	width1 = ifelse(layout == "1|(2-3)", 800, 450), 
	height1 = ifelse(layout == "1-(2|3)", 700, 350), 
	width2 = 400, 
	height2 = 350, 
	width3 = ifelse(layout == "(1-2)|3", 800, 400),
	layout = ifelse("brush" %in% response, "(1-2)|3", "1-3"), compact = FALSE,
	action = "click", cursor = TRUE, response = c(action, "brush"),
	brush_opt = list(stroke = "#f00", opacity = 0.6),
	output_ui_float = FALSE,

	# specific for sub-heatmap
	sub_heatmap_cell_fun = NULL, sub_heatmap_layer_fun = NULL,

	save = NULL
	) {

	if(is.null(ht_list)) {
		if(length(dev.list())) {
			stop_wrap("No heatmap is detected. Detected there is opened graphics device. If the heatmap was already made in that device, enter `ComplexHeatmap::ht_opt(save_last = TRUE)`, regenerate the heatmap and run `htShiny()` again.")
		} else {
			stop_wrap("No heatmap is detected.")
		}
	} else if(inherits(ht_list, "InputHeatmap")) {
		ht_list = show(ht_list)
	} else if(inherits(ht_list, "matrix")) {
		stop_wrap("No heatmap is detected. Maybe you forgot to use `Heatmap()`?")
	} else {
		if(is.numeric(ht_list) && length(ht_list) == 1) {
			stop_wrap("Maybe you want to use the function `htShinyExample()`?")
		}
	}

	if(is.null(title)) {
		title = "InteractiveComplexHeatmap Shiny App"
	}
	if(is.character(title)) {
		title = titlePanel(title)
	}
	
	if(is.null(description)) {
		description = "You can click a position or select an area from the heatmap. The original heatmap and the selected sub-heatmap can be resized by dragging from the bottom right of the box."
	}
	if(is.character(description)) {
		description = p(description)
	}

	if(is.character(html)) {
		html = HTML(html)
	}

	if(missing(width1) && missing(height1)) {
		if(inherits(ht_list, "HeatmapList")) {
			if(ht_list@layout$initialized) {
				
				width_ht = ComplexHeatmap:::width(ht_list)	
				height_ht = ComplexHeatmap:::height(ht_list)

			    if(is_abs_unit(width_ht) && is_abs_unit(height_ht)) {
		    		width_ht = ceiling(convertWidth(width_ht, "bigpts", valueOnly = TRUE))
		    		height_ht = ceiling(convertHeight(height_ht, "bigpts", valueOnly = TRUE))

		    		width1 = width_ht
		    		height1 = height_ht
		    	}
		    }
		} 
	}

	if(!is.null(save)) {
		if(file.exists(save)) {
			if(!file.info(save)["isdir"]) {
				stop_wrap("`save` should be a folder.")
			}
		}
		dir.create(save, showWarnings = FALSE)

		save(list = ls(), file = paste0(save, "/htShiny.RData"))
		code = "
load('htShiny.RData')
chooseCRANmirror(ind = 1)
setRepositories(ind = 1:2)
if(!requireNamespace('InteractiveComplexHeatmap', quietly = TRUE)) {
	install.packages('InteractiveComplexHeatmap')
}
library(shiny)
suppressPackageStartupMessages(library(InteractiveComplexHeatmap))
suppressPackageStartupMessages(library(ComplexHeatmap))
ui = fluidPage(
	title,
	description,
	if(hline) hr() else NULL,
	InteractiveComplexHeatmapOutput(heatmap_id = heatmap_id, title1 = title1, title2 = title2,
		width1 = width1, height1 = height1, width2 = width2, height2 = height2, layout = layout, compact = compact,
		action = action, cursor = cursor, response = response, brush_opt = brush_opt, output_ui_float = output_ui_float), 
	html
)

server = function(input, output, session) {
	makeInteractiveComplexHeatmap(input, output, session, ht_list, 
		sub_heatmap_cell_fun = sub_heatmap_cell_fun, sub_heatmap_layer_fun = sub_heatmap_layer_fun)
}

cat('If the shiny app is not automatically opened in the browser, you can manually copy the following link and paste it to the browser.');
print(shinyApp(ui, server))
"		
		writeLines(code, con = paste0(save, "/htShiny.R"))
		writeLines("Rscript htShiny.R", con = paste0(save, "/htShiny.sh"))
		writeLines("Rscript htShiny.R", con = paste0(save, "/htShiny.bat"))
		return(invisible(NULL))
	}

	ui = fluidPage(
		title,
		description,
		if(hline) hr() else NULL,
		InteractiveComplexHeatmapOutput(heatmap_id = heatmap_id, title1 = title1, title2 = title2,
			width1 = width1, height1 = height1, width2 = width2, height2 = height2, layout = layout, compact = compact,
			action = action, cursor = cursor, response = response, brush_opt = brush_opt, output_ui_float = output_ui_float), 
		html
	)

	server = function(input, output, session) {
		makeInteractiveComplexHeatmap(input, output, session, ht_list, 
			sub_heatmap_cell_fun = sub_heatmap_cell_fun, sub_heatmap_layer_fun = sub_heatmap_layer_fun)
	}

	shinyApp(ui, server)
}

# == title
# Interactive heatmaps as a Shiny app
#
# == param
# -... All goes to `htShiny`.
#
# == value
# A Shiny app object.
#
ht_shiny = function(...) {
	htShiny(...)
}


