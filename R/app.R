
# == title
# Interactive heatmaps with a shiny app
#
# == param
# -ht_list A `ComplexHeatmap::Heatmap-class` or a `ComplexHeatmap::HeatmapList-class` object. If it is not specified, a random heatmap is used.
#     Better already updated by ``draw()`` function.
# -title Title of the app.
# -description Description of the app.
# -hline Whether to add the horizontal line (by ``hr`` tag).
# -html HTML fragment inserted below the heatmap.
# -... Pass to `InteractiveComplexHeatmapOutput`.
#
# == seealso
# https://jokergoo.shinyapps.io/interactive_complexHeatmap/
#
# == value
# A shiny app object.
#
# == example
# # use a random heatmap
# if(interactive()) {
# htShiny()
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
# htShiny(ht)
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
# htShiny(ht_list)
# 
# ht_list = ht1 \%v\% ht2
# htShiny(ht_list)
# }
htShiny = function(ht_list = get_last_ht(), title = NULL, description = NULL, 
	hline = TRUE, html = NULL, ...) {

	if(is.null(ht_list)) {
		if(length(dev.list())) {
			stop_wrap("No heatmap is detected. Detected there is opened graphics device. If the heatmap was already made in that device, enter `ComplexHeatmap::ht_opt$save_last = TRUE` and run `htShiny()` again.")
		} else {
			stop_wrap("No heatmap is detected.")
		}
	} else if(inherits(ht_list, "InputHeatmap")) {
		ht_list = show(ht_list)
	} else {
		if(is.numeric(ht_list)) {
			stop_wrap("Maybe you are looking for `ht_shiny_example()`?")
		}
	}

	if(is.null(title)) {
		title = "ComplexHeatmap Shiny App"
	}
	if(is.character(title)) {
		title = titlePanel(title)
	}
	
	if(is.null(description)) {
		description = "You can click a position or select an area from the heatmap. The original heatmap and the selected sub-heatmap can be resized by dragging from the bottom right of the box. If the heatmap is too huge or you resize the heatmap too frequently, the heatmap might not be correctly updated. You can just slightly resize the heatmap again and wait for several seconds."
	}
	if(is.character(description)) {
		description = p(description)
	}

	if(is.character(html)) {
		html = HTML(html)
	}

	ui = fluidPage(
		title,
		description,
		if(hline) hr() else NULL,
		InteractiveComplexHeatmapOutput(...), 
		html
	)

	server = function(input, output, session) {
		renderInteractiveComplexHeatmap(ht_list, input, output, session)
	}

	shiny::shinyApp(ui, server)
}

# == title
# Interactive heatmaps with a shiny app
#
# == param
# -... All goes to `htShiny`.
#
# == value
# A shiny app object.
#
ht_shiny = function(...) {
	htShiny(...)
}

# == title
# Examples of the interactive complex heatmaps
#
# == param
# -which An integer of which example to use. The list of all examples can be obtained by executing `htShinyExample` with no argument.
#
# == details
# The source code of all examples are in ``systm.file("examples", "examples.R")``.
#
# == value
# A shiny app object.
#
# == example
# htShinyExample()
# if(interactive()) {
#     htShinyExample(4)
# }
htShinyExample = function(which) {
	examples = get_examples()
	if(missing(which)) {
		cat("There are following examples:\n\n")
		title = vapply(examples, function(x) x$title, "")
		for(i in seq_along(title)) {
			lines = strwrap(title[i], width = getOption("width") - 5)
			lines[1] = paste0(ifelse(nchar(i) == 1, " ", ""), i, ". ", lines[1])
			lines[-1] = paste0(strrep(" ", nchar(i)+2), lines[-1])
			cat(paste(lines, collapse = "\n"))
			cat("\n")
		}
		cat("\n")
	} else {
		which = which[1]

		if(which > length(examples) || which < 1) {
			stop(qq("The value of `which` should be between 1 and @{length(examples)}."))
		}
		code = examples[[which]]$code
		title = examples[[which]]$title

		k = which(grepl("rmarkdown::run\\(", code))
		if(length(k)) {
			eval(parse(text = code[k]))
			return(invisible(NULL))
		}

		library_calls = code[grepl("(library|require)\\(.*?\\)", code)]
		if(length(library_calls)) {
			required_pkgs = gsub("^.*(library|require)\\(([^)]*)\\).*$", "\\2", library_calls)
			loaded_pkgs = search()
			loaded_pkgs = loaded_pkgs[grepl("^package", loaded_pkgs)]
			loaded_pkgs = gsub("^package:", "", loaded_pkgs)
			for(pkg in required_pkgs) {
				if(!requireNamespace(pkg, quietly = TRUE)) {
					stop(paste0("Package '", pkg, "' should be installed for running this example."))
				}
				if(!pkg %in% loaded_pkgs) {
					message(paste0("Note: Namespace 'package:", pkg, "' is inserted into the search list. It might bring conflicts to some functions."))
				}
			}
		}
		message("Processing the heatmaps. It takes different time depending on examples...\n")

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
<pre>
@{code2}
</pre>
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
HTML('<hr /><div>
<h3>Information of this Shiny app<h3>
<h5>Description</h5>
<pre>@{title}</pre>
<h5>Source code</h5>
<pre>
@{code_line}
</pre>
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
		text = readLines("~/project/InteractiveComplexHeatmap/inst/examples/examples.R")
	} else {
		text = readLines(system.file("examples", "examples.R", package = "InteractiveComplexHeatmap"))
	}

	text = text[!grepl("^#{10,}$", text)]

	ind = which(grepl("^#+\\s*title:", text))
	ind2 = c(ind[-1] - 1, length(text))

	examples = list()
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
		examples[[i]] = list(
			title = title, 
			code = code
		)
	}

	examples
}
