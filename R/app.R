
# == title
# Interactive heatmaps as a Shiny app
#
# == param
# -ht_list A `ComplexHeatmap::Heatmap-class` or a `ComplexHeatmap::HeatmapList-class` object. If it is not specified, the last generated heatmap is used.
#     Better already updated by ``draw()`` function.
# -title Title of the app.
# -description Description of the app. The content will be wrapped by a ``p`` tag and inserted before the interactive heatmap widget.
# -hline Whether to add the horizontal line (by ``hr`` tag) after ``description``.
# -html HTML fragment inserted below the heatmap.
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
# There are also many examples with `htShinyExample`.
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
	output_ui_float = FALSE

	) {

	if(is.null(ht_list)) {
		if(length(dev.list())) {
			stop_wrap("No heatmap is detected. Detected there is opened graphics device. If the heatmap was already made in that device, enter `ComplexHeatmap::ht_opt(save_last = TRUE)`, regenerate the heatmap and run `htShiny()` again.")
		} else {
			stop_wrap("No heatmap is detected.")
		}
	} else if(inherits(ht_list, "InputHeatmap")) {
		ht_list = show(ht_list)
	} else {
		if(is.numeric(ht_list) && length(ht_list) == 1) {
			stop_wrap("Maybe you want to use the function `htShinyExample()`?")
		}
	}

	if(is.null(title)) {
		title = "ComplexHeatmap Shiny App"
	}
	if(is.character(title)) {
		title = titlePanel(title)
	}
	
	if(is.null(description)) {
		description = "You can click a position or select an area from the heatmap. The original heatmap and the selected sub-heatmap can be resized by dragging from the bottom right of the box. "
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
		makeInteractiveComplexHeatmap(input, output, session, ht_list)
	}

	shiny::shinyApp(ui, server)
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

# == title
# Examples of interactive complex heatmaps
#
# == param
# -which An index of which example to use. The list of all examples can be obtained by executing `htShinyExample` with no argument.
#
# == details
# In every example, there is a Shiny app opened, also including source code that generates this app.
#
# == value
# A Shiny app object.
#
# == example
# # list all examples
# htShinyExample()
#
# if(interactive()) {
#     htShinyExample(4.2)
# }
htShinyExample = function(which) {
	if(missing(which)) {
		cat("There are following examples. Individual example can be run by e.g. htShinyExample(1.1).\n\n")
		for(i_cate in seq_along(examples)) {
			category = examples[[i_cate]]$category
			cat(strrep(clisymbols::symbol$line, 8), paste0(i_cate, "."), category, strrep(clisymbols::symbol$line, getOption("width") - 8 - nchar(category)), "\n")

			e = examples[[i_cate]]$example
			title = vapply(e, function(x) x$title, "")
			for(i in seq_along(title)) {
				lines = strwrap(title[i], width = getOption("width") - 5)
				lines[1] = paste0(" ", i_cate, ".", i, " ", lines[1])
				lines[-1] = paste0(strrep(" ", 1 + nchar(i_cate) + 1 + nchar(i) + 1), lines[-1])
				cat(paste(lines, collapse = "\n"))
				cat("\n")
			}
			cat("\n")
		}
	} else {
		which = as.character(which[1])
		ind = as.numeric(strsplit(which, "\\.")[[1]])
		if(length(ind) == 1) {
			ind = c(ind, 1)
		}

		i_cate = ind[1]
		i = ind[2]
		code = examples[[i_cate]]$example[[i]]$code
		title = examples[[i_cate]]$example[[i]]$title
		
		k = which(grepl("rmarkdown::run\\(", code))
		if(length(k)) {
			eval(parse(text = code[k]))
			return(invisible(NULL))
		}
		
		version = packageDescription('InteractiveComplexHeatmap', fields = "Version")

		library_calls = code[grepl("(library|require)\\(.*?\\)", code)]
		if(length(library_calls)) {
			required_pkgs = gsub("^.*(library|require)\\(([^)]*)\\).*$", "\\2", library_calls)
			loaded_pkgs = search()
			loaded_pkgs = loaded_pkgs[grepl("^package", loaded_pkgs)]
			loaded_pkgs = gsub("^package:", "", loaded_pkgs)
			for(pkg in required_pkgs) {
				
				check_pkg(pkg)
				
				if(!pkg %in% loaded_pkgs) {
					msg = paste0("Note: Namespace 'package:", pkg, "' is inserted into the search list. It might bring conflicts to some functions.")
					msg = strwrap(msg)
					msg[-1] = paste0("  ", msg[-1])
					message(paste(msg, collapse = "\n"))
				}
			}
		}
		message("Processing the heatmaps. It takes different time depending on examples...")

		if(any(grepl("htShiny\\(", code))) {

			code2 = paste(code, collapse = "\n")
			code2 = gsub("^\\s+||\\s+$", "", code2)

			original_htShiny = htShiny

			htShiny = function(ht, ...) {
				html = qq("
<hr />
<div>
<h3>Information of this Shiny app<h3>
<h5>Description</h5>
<pre>@{title}</pre>
<h5>Source code</h5>
<pre id=\"code\">
@{code2}
</pre>
<script>
create_clipboard(\"code\");
</script>
<hr />
<p>Generated by <a href=\"https://github.com/jokergoo/InteractiveComplexHeatmap\" target=\"_blank\">InteractiveComplexHeatmap</a> version @{version}</p>
</div>")
				original_htShiny(ht, ..., html = HTML(html))
			}
		} else {
			i = which(grepl("shinyApp\\(", code))
			code2 = c(code[seq_len(i-1)], "", code[seq(i, length(code))])
			code_line = paste(code, collapse = '\n')
			code_line = gsub("'", "\\\\'", code_line)
			code_line = gsub("<", "&lt;", code_line)
			code_line = gsub(">", "&gt;", code_line)
			code2[i] = qq("
ui = fluidPage(
    ui,
HTML('<hr /><div style=\"clear:both;\">
<h3>Information of this Shiny app<h3>
<h5>Description</h5>
<pre>@{title}</pre>
<h5>Source code</h5>
<pre id=\"code\">
@{code_line}
</pre>
<script>
create_clipboard(\"code\");
</script>
<hr />
<p>Generated by <a href=\"https://github.com/jokergoo/InteractiveComplexHeatmap\" target=\"_blank\">InteractiveComplexHeatmap</a> version @{version}</p>
</div>')
)
")			
			code = code2
		}

		oe = try({
			dev.null()
			app = eval(parse(text = code))
		})
		dev.off2()
		if(inherits(oe, "try-error")) {
			stop(oe)
		} else {
			app
		}
	}
}

get_examples = function() {

	if(identical(topenv(), .GlobalEnv)) {
		example_dir = "~/project/development/InteractiveComplexHeatmap/inst/examples"
	} else {
		example_dir = system.file("examples", package = "InteractiveComplexHeatmap")
	}

	example_files = list.files(path = example_dir, pattern = "example", full.names = TRUE)

	examples = list()
	ie = 0

	for(i_file in seq_along(example_files)) {

		examples[[i_file]] = list()

		text = readLines(example_files[i_file])

		category = text[1]
		category = gsub("^\\s*#\\s*", "", category)
		text = text[-1]
		examples[[i_file]]$category = category

		text = text[!grepl("^#{10,}$", text)]

		ind = which(grepl("^#+\\s*title:", text))
		ind2 = c(ind[-1] - 1, length(text))

		examples[[i_file]]$example = list()
		
		for(i in seq_along(ind)) {

			code = text[seq(ind[i]+1, ind2[i])]
			if(!any(grepl("(library|require)\\(ComplexHeatmap\\)", code))) {
				code = c("suppressPackageStartupMessages(library(ComplexHeatmap))", code)
			}
			if(!any(grepl("(library|require)\\(InteractiveComplexHeatmap\\)", code))) {
				code = c("suppressPackageStartupMessages(library(InteractiveComplexHeatmap))", code)
			}

			for(k in rev(seq_along(code))) {
				if(grepl("^\\s*$", code[k])) {
					code = code[-k]
				} else {
					break
				}
			}
			title = gsub("^#+\\s*title:\\s+", "", text[ind[i]])
			code = gsub("\\t", "    ", code)

			ie = ie + 1
			examples[[i_file]]$example[[i]] = list(
				title = title, 
				index = ie,
				code = code
			)
		}
	}

	examples
}


examples = get_examples()
