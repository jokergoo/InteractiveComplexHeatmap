
# == title
# Interactive heatmaps with a shiny app
#
# == param
# -ht_list A `ComplexHeatmap::Heatmap-class` or a `ComplexHeatmap::HeatmapList-class` object. If it is not specified, a random heatmap is used.
#     Better be updated by ``draw()`` function.
# -... Pass to `InteractiveComplexHeatmapOutput`.
# -html HTML fragment inserted below the heatmap.
#
# == seealso
# https://jokergoo.shinyapps.io/interactive_complexHeatmap/
#
# == example
# # use a random heatmap
# if(interactive()) {
# ht_shiny()
# }
#
# # by providing a heatmap/heatmap list
# if(interactive()) {
# m = matrix(rnorm(100), 10)
# rownames(m) = 1:10
# colnames(m) = 1:10
# 
# ht = Heatmap(m)
# ht = draw(ht)
# ht_shiny(ht)
# }
#
# if(interactive()) {
# m1 = matrix(rnorm(100), 10)
# rownames(m1) = 1:10
# colnames(m1) = 1:10
# ht1 = Heatmap(m1, row_km = 2, column_km = 2)
# 
# m2 = matrix(sample(letters[1:10], 100, replace = TRUE), 10)
# ht2 = Heatmap(m2)
#  
# ht_list = draw(ht1 + ht2)
# ht_shiny(ht_list)
# 
# ht_list = ht1 \%v\% ht2
# ht_shiny(ht_list)
# }
ht_shiny = function(ht_list, ..., html = NULL) {
	dev.null(NULL)
	oe = try({
		if(missing(ht_list)) {
			cat("No heatmap is provided, use random heatmap\n")
		    m1 = matrix(rnorm(100), 10)
		    colnames(m1) = rownames(m1) = paste0("a", 1:10)
		    ht1 = Heatmap(m1, row_km = 2, column_km = 2)

		    m2 = matrix(sample(letters[1:10], 100, replace = TRUE), 10)
		    colnames(m2) = rownames(m2) = paste0("b", 1:10)
		    ht2 = Heatmap(m2, heatmap_legend_param = list(at = sort(unique(as.vector(m2)))))
		    ht_list = draw(ht1 + ht2)
		} else if(inherits(ht_list, "InputHeatmap")) {
			ht_list = show(ht_list)
		}
	}, silent = TRUE)
	dev.off2()
	if(inherits(oe, "try-error")) {
		stop(oe)
	}
	
	has_normal_matrix = FALSE
	if(inherits(ht_list, "Heatmap")) {
		if(nrow(ht_list@matrix) > 0 && ncol(ht_list@matrix) > 0) {
			has_normal_matrix = TRUE
		}
	} else {
		for(i in seq_along(ht_list@ht_list)) {
			if(inherits(ht_list@ht_list[[i]], "Heatmap")) {
				ht = ht_list@ht_list[[i]]
				
				if(nrow(ht@matrix) == 0 || ncol(ht@matrix) == 0) {
					next
				} else {
					has_normal_matrix = TRUE
					break
				}
			}
		}
	}
	if(!has_normal_matrix) {
		stop_wrap("There should be one normal heatmap (nrow > 0 and ncol > 0) in the heatmap list.")
	}

	
	ui = fluidPage(
		titlePanel("ComplexHeatmap Shiny App"),

		p("You can click a position or select an area from the heatmap(s). The original heatmap and the selected sub-heatmap can be resized by dragging from the bottom right. If the heatmap is too huge or you resize the heatmap too frequently, the heatmap might not be correctly updated. You can just need to slightly resize the heatmap again and wait for several seconds."),
		hr(),

		InteractiveComplexHeatmapOutput(...), 
		html
	)

	server = function(input, output, session) {
		MakeInteractiveComplexHeatmap(ht_list, input, output, session)
	}

	shiny::shinyApp(ui, server)
}

# == title
# Examples of the interactive complex heatmaps
#
# == param
# -which an integer of which example to use. The list of all examples can be obtained by calling `ht_shiny_example` which no argument.
#
# == details
# The source code of all examples are in ``systm.file("examples", "examples.R")``.
#
ht_shiny_example = function(which) {
	examples = get_examples()
	if(missing(which)) {
		cat("There are following examples:\n\n")
		title = sapply(examples, function(x) x$title)
		for(i in seq_along(title)) {
			lines = strwrap(title[i], width = 0.9 * getOption("width") - 5)
			lines[1] = paste0(ifelse(nchar(i) == 1, " ", ""), i, ". ", lines[1])
			lines[-1] = paste0(strrep(" ", nchar(i)+2), lines[-1])
			cat(paste(lines, collapse = "\n"))
			cat("\n")
		}
		cat("\n")
	} else {
		which = which[1]
		code = examples[[which]]$code

		library_calls = code[grepl("library\\(.*\\)", code)]
		if(length(library_calls)) {
			required_pkgs = gsub("library\\((.*)\\)", "\\1", library_calls)
			for(pkg in required_pkgs) {
				if(!requireNamespace(pkg, quietly = TRUE)) {
					stop_wrap(paste0("Package", pkg, "should be installed for running this example."))
				}
			}
		}
		cat("Processing the heatmaps... It takes different time depending on examples.\n")

		if(any(grepl("ht_shiny\\(", code))) {

			code2 = paste(code, collapse = "\n")
			code2 = gsub("^\\s+||\\s+$", "", code2)

			original_ht_shiny = ht_shiny

			ht_shiny = function(ht, ...) {
			html = qq("
<hr />
<div>
<h4>Source code for exporting this Shiny app</h4>
<pre>
@{code2}
</pre>
</div>")
				original_ht_shiny(ht, ..., html = HTML(html))
			}
		} else {
			i = which(grepl("shinyApp\\(", code))
			code2 = c(code[1:(i-1)], "", code[seq(i, length(code))])
			code_line = paste(code, collapse = '\n')
			code_line = gsub("'", "\\\\'", code_line)
			code_line = gsub("<", "&lt;", code_line)
			code_line = gsub(">", "&gt;", code_line)
			code2[i] = qq("
ui = fluidPage(
    ui,
HTML('<hr /><div>
<h4>Source code for exporting this Shiny app</h4>
<pre>
@{code_line}
</pre>
</div>')
)
")			
			code = code2
		}

		eval(parse(text = code))
	}
}

get_examples = function() {
	if(identical(topenv(), .GlobalEnv)) {
		text = readLines("~/project/InteractiveComplexHeatmap/inst/examples/examples.R")
	} else {
		text = readLines(system.file("examples", "examples.R", package = "InteractiveComplexHeatmap"))
	}
	ind = which(grepl("^#+\\s*title:", text))
	ind2 = c(ind[-1] - 1, length(text))

	examples = list()
	for(i in seq_along(ind)) {
		code = text[seq(ind[i]+1, ind2[i])]
		if(!any(grepl("library\\(InteractiveComplexHeatmap\\)", code))) {
			code = c("library(InteractiveComplexHeatmap)", code)
		}

		for(k in rev(seq_along(code))) {
			if(grepl("^\\s*$", code[k])) {
				code = code[-k]
			} else {
				break
			}
		}
		code = gsub("\\t", "    ", code)
		examples[[i]] = list(title = gsub("^#+\\s*title:\\s+", "", text[ind[i]]), 
			code = code
		)
	}

	examples
}
