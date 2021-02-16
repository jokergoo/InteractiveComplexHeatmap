
# == title
# UI for the interactive ComplexHeatmap
#
# == param
# -heatmap_id ID of the plot. If it is not specified, an internal ID is assigned.
# -title1 Title of the original heatmap.
# -title2 Title of the sub-heatmap.
# -width1 Width of the original heatmap.
# -height1 Height of the original heatmap.
# -width2 Width of the sub-heatmap.
# -height2 Height of the sub-heatmap.
# -width3 Width of the output div.
# -layout One of ``(1|2)-3``, ``1-(2|3)``, ``1-2-3``, ``1|2|3``, ``1|(2-3)``.
# -action Which action for selecting single cell on the heatmap? Value should be ``click``, ``hover`` or ``dblclick``.
# -brush_opt A list of parameters passed to `shiny::brushOpts`.
# -output_ui Whether to add the output ``div``.
# -css Self-defined CSS code.
# -... Pass to the ui container.
#
# == details
# This function generates HTML fragment for the interactive UI. See the example from `makeInteractiveComplexHeatmap` page.
#
# == value
# A UI that can be used in shiny.
InteractiveComplexHeatmapOutput = function(heatmap_id = NULL, 
	title1 = "Original heatmap", title2 = "Selected sub-heatmap",
	width1 = ifelse(layout == "1|(2-3)", 800, 450), 
	height1 = ifelse(layout == "1-(2|3)", 700, 350), 
	width2 = 370, 
	height2 = 350, 
	width3 = ifelse(layout == "(1-2)|3", 800, 370),
	layout = "(1-2)|3",
	action = "click", 
	brush_opt = list(stroke = "#f00", opacity = 0.6), 
	output_ui = default_output_ui(), css = "", ...) {

	if(is.null(heatmap_id)) {
		increase_widget_index()
		heatmap_id = paste0("ht", get_widget_index())
	}

	if(grepl("^\\d", heatmap_id)) {
		stop_wrap("heatmap_id cannot start with digits.")
	}
	if(!grepl("^[a-zA-Z0-9_]+$", heatmap_id)) {
		stop_wrap("heatmap_id can only contain letters, numbers and underlines.")
	}

	shiny_env[[heatmap_id]] = list()
	shiny_env$current_heatmap_id = heatmap_id

	action = match.arg(action)[1]
	if(action == "dblclick") {
		click = NULL
		dblclick = qq("@{heatmap_id}_heatmap_click")
		hover = NULL
	} else if(action == "hover") {
		click = NULL
		dblclick = NULL
		hover = qq("@{heatmap_id}_heatmap_click")
	} else {
		click = qq("@{heatmap_id}_heatmap_click")
		dblclick = NULL
		hover = NULL
	}

	if(is.null(css)) {css = ""}
	css[is.na(css)] = ""

	if(file.exists(css)) {
		css = paste(readLines(css), collapse = "\n")
	} else {
		css = paste(css, collapse = "\n")
	}

	jqueryui_dep = htmltools::htmlDependency(
		name       = "jqueryui",
		version    = "1.12.1",
		package    = "shiny",
		src        = "www/shared/jqueryui",
		script     = "jquery-ui.min.js",
		stylesheet = "jquery-ui.min.css"
    )

    pickr_dep = htmltools::htmlDependency(
		name       = "pickr",
		version    = "1.8.0",
		package    = "InteractiveComplexHeatmap",
		src        = "www",
		script     = c("pickr.min.js", "pickr.es5.min.js"),
		stylesheet = c("classic.min.css", "monolith.min.css", "nano.min.css")
	)

	fontawesome_dep = htmltools::htmlDependency(
		name       = "fontawesome",
		version    = "5.13.0",
		package    = "shiny",
		src        = "www/shared/fontawesome/css",
		stylesheet = c("all.min.css", "v4-shims.min.css")
    )

    if(is.null(brush_opt$fill)) {
    	pickr_fill = "#003366"
    } else {
    	pickr_fill = brush_opt$fill
    }
    if(is.null(brush_opt$stroke)) {
    	pickr_border = "#99ccff"
    } else {
    	pickr_border = brush_opt$stroke
    }
    if(is.null(brush_opt$opacity)) {
    	pickr_opacity = 0.25
    } else {
    	pickr_opacity = brush_opt$opacity
    }

	td = tempdir()
	if(identical(topenv(), .GlobalEnv)) {
    	ht_js = paste(readLines("~/project/InteractiveComplexHeatmap/inst/template/ht.js"), collapse = "\n")
    } else {
    	ht_js = paste(readLines(system.file("template", "ht.js", package = "InteractiveComplexHeatmap")), collapse = "\n")
    }
    temp_js = tempfile(fileext = ".js", tmpdir = td)
    writeLines(qq(ht_js), con = temp_js)

    if(identical(topenv(), .GlobalEnv)) {
    	ht_css = paste(readLines("~/project/InteractiveComplexHeatmap/inst/template/ht.css"), collapse = "\n")
    } else {
		ht_css = paste(readLines(system.file("template", "ht.css", package = "InteractiveComplexHeatmap")), collapse = "\n")
    }
    temp_css = tempfile(fileext = ".css", tmpdir = td)
    writeLines(qq(ht_css), con = temp_css)

    ht_js_dep = htmltools::htmlDependency(
		name   = qq("js_@{heatmap_id}"),
		version = "0.0.1",
		src = td,
		stylesheet = basename(temp_css),
		script = basename(temp_js)
    )

    main_heatmap_ui = div(id = qq("@{heatmap_id}_heatmap_group"),
		h5(title1),
		div(id = qq("@{heatmap_id}_heatmap_resize"),
			plotOutput(qq("@{heatmap_id}_heatmap"), height = height1, width = width1,
				        brush = do.call(brushOpts, c(list(id = qq("@{heatmap_id}_heatmap_brush")), brush_opt)),
				        click = click, dblclick = dblclick, hover = hover
			),
			tags$script(HTML(qq("
				$('#@{heatmap_id}_heatmap').html('<p style=\"position:relative;top:50%;\">Making heatmap, please wait...</p>');
			")))
		),
		div(id = qq("@{heatmap_id}_heatmap_control"),
			style = "display:none;",
			tabsetPanel(
				tabPanel(HTML("<i class='fa fa-search'></i>"),
					div(id = qq('@{heatmap_id}_tabs-search'), 
						div(textInput(qq("@{heatmap_id}_keyword"), placeholder = "Multiple keywords separated by ','", label = "Keywords"), style = "width:250px;float:left;"),
						div(checkboxInput(qq("@{heatmap_id}_search_regexpr"), label = "Regular expression", value = FALSE), style = "width:150px;float:left;padding-top:20px;padding-left:4px;"),
						div(style = "clear: both;"),
						radioButtons(qq("@{heatmap_id}_search_where"), label = "Which dimension to search?", choices = list("on rows" = 1, "on columns" = 2), selected = 1, inline = TRUE),
						checkboxGroupInput(qq("@{heatmap_id}_search_heatmaps"), label = "Which heatmaps to search?", choiceNames = "loading", choiceValues = "", selected = ""),
						checkboxGroupInput(qq("@{heatmap_id}_search_extend"), label = "Extend sub-heatmap to all heatmaps and annotations?", choiceNames = "yes", choiceValues = 1, selected = NULL),
						actionButton(qq("@{heatmap_id}_search_action"), label = "Search")
					),
					p("Search Heatmap", style = "display:none;")
				),
				tabPanel(HTML("<i class='fa fa-brush'></i>"),
					div(
						id = qq('@{heatmap_id}_tabs-brush'),
						HTML(qq('
							<div class="form-group shiny-input-container" style="float:left; width:120px;">
							<label>Brush border</label>
							<div id="@{heatmap_id}_color_pickers_border"></div>
							</div>
							<div class="form-group shiny-input-container" style="float:left; width:120px;">
							<label>Brush fill</label>
							<div id="@{heatmap_id}_color_pickers_fill"></div>
							</div>
							<div style="clear:both;"></div>')
						),
						selectizeInput(qq("@{heatmap_id}_color_pickers_border_width"), label = "Border width", 
							choices = list("1px" = 1, "2px" = 2, "3px" = 3), selected = 1,
							options = list(render = I("{
									option: function(item, escape) {
										return '<div><hr style=\"border-top:' + item.value + 'px solid black;\"></div>'
									}
								}"))),
						sliderInput(qq("@{heatmap_id}_color_pickers_opacity"), label = "Opacity", min = 0, max = 1, value = pickr_opacity)
					)
				),
				tabPanel(HTML("<i class='fa fa-images'></i>"),
					div(
						id = qq('@{heatmap_id}_tabs-save-image'),
						radioButtons(qq("@{heatmap_id}_heatmap_download_format"), label = "Which format?", choices = list("png" = 1, "pdf" = 2, "svg" = 3), selected = 1, inline = TRUE),
						numericInput(qq("@{heatmap_id}_heatmap_download_image_width"), label = "Image width (in px)", value = width1),
						numericInput(qq("@{heatmap_id}_heatmap_download_image_height"), label = "Image height (in px)", value = height1),
						downloadButton(qq("@{heatmap_id}_heatmap_download_button"), "Save image")
					)
				),
				tabPanel(HTML("<i class='fa fa-expand-arrows-alt'></i>"),
					div(id = qq('@{heatmap_id}_tabs-resize'),
						numericInput(qq("@{heatmap_id}_heatmap_input_width"), "Box width", width1),
						numericInput(qq("@{heatmap_id}_heatmap_input_height"), "Box height", height1),
						actionButton(qq("@{heatmap_id}_heatmap_input_size_button"), "Change image size")
					)
				)
			)
		)
	)

	sub_heatmap_ui = div(id = qq("@{heatmap_id}_sub_heatmap_group"),
		h5(title2),
		div(id = qq("@{heatmap_id}_sub_heatmap_resize"),
			plotOutput(qq("@{heatmap_id}_sub_heatmap"), height = height2, width = width2)
		),
		div(id = qq("@{heatmap_id}_sub_heatmap_control"),
			style = "display:none;",
			tabsetPanel(
				tabPanel(HTML("<i class='fa fa-tasks'></i>"),
					div(id = qq('@{heatmap_id}_sub_tabs-setting'), 
						div(
							div(checkboxInput(qq("@{heatmap_id}_show_row_names_checkbox"), label = "Show row names", value = TRUE), style = "float:left;width:150px"),
							div(checkboxInput(qq("@{heatmap_id}_show_column_names_checkbox"), label = "Show column names", value = TRUE), style = "float:left;width:160px"),
							div(style = "clear: both;")
						),
						div(
							checkboxInput(qq("@{heatmap_id}_show_annotation_checkbox"), label = "Show heatmap annotations", value = TRUE),
							checkboxInput(qq("@{heatmap_id}_show_cell_fun_checkbox"), label = "Show cell decorations", value = TRUE),
							checkboxInput(qq("@{heatmap_id}_fill_figure_checkbox"), label = "Fill figure region", value = FALSE)
						),
						hr(),
						div(
							HTML(qq('
						<p>
						Remove <input id="@{heatmap_id}_post_remove" type="number" class="form-control" min="1" value="1" style="width:60px;display:inline;"/>
						<span id="@{heatmap_id}_post_remove_which">rows</span> from 
						<select id="@{heatmap_id}_post_remove_dimension" class="form-control" style="width:auto;display:inline;">
						<option value="top" selected>top</option>
						<option value="bottom">bottom</option>
						<option value="left">left</option>
						<option value="right">right</option></select>
						</p>
							')),
							actionButton(qq("@{heatmap_id}_post_remove_submit"), "Remove"),
							actionButton(qq("@{heatmap_id}_post_remove_reset"), "Reset"),
							tags$script(HTML(qq("
								$('#@{heatmap_id}_post_remove_dimension').change(function() {
									if($(this).val() == 1 || $(this).val() == 2) {
										$('#@{heatmap_id}_post_remove_which').text('rows');
									} else {
										$('#@{heatmap_id}_post_remove_which').text('columns');
									}
								});
							")))
						),
						hr(),
						p("Click the button below to turn the sub-heatmap into an interactive app.", style = "max-width:300px;"),
						actionButton(qq("@{heatmap_id}_open_modal"), label = "Interactivate sub-heatmap")
					)
				),
				tabPanel(HTML("<i class='fa fa-table'></i>"),
					div(id = qq("@{heatmap_id}_sub_tabs-table"),
						p("Export values in sub-heatmaps as a text table."),
						actionButton(qq("@{heatmap_id}_open_table"), label = "Open table")
					)
				),
				tabPanel(HTML("<i class='fa fa-images'></i>"),
					div(id = qq('@{heatmap_id}_sub_tabs-save-image'),
						radioButtons(qq("@{heatmap_id}_sub_heatmap_download_format"), label = "Which format?", choices = list("png" = 1, "pdf" = 2, "svg" = 3), selected = 1, inline = TRUE),
						numericInput(qq("@{heatmap_id}_sub_heatmap_download_image_width"), label = "Image width (in px)", value = width2),
						numericInput(qq("@{heatmap_id}_sub_heatmap_download_image_height"), label = "Image height (in px)", value = height2),
						downloadButton(qq("@{heatmap_id}_sub_heatmap_download_button"), "Save image")
					),
				),
				tabPanel(HTML("<i class='fa fa-expand-arrows-alt'></i>"),
					div(id = qq('@{heatmap_id}_sub_tabs-resize'),
						numericInput(qq("@{heatmap_id}_sub_heatmap_input_width"), "Box width", width2),
						numericInput(qq("@{heatmap_id}_sub_heatmap_input_height"), "Box height", height2),
						actionButton(qq("@{heatmap_id}_sub_heatmap_input_size_button"), "Change image size")
					)
				)
			)
		)
	)

	default_output_ui = function() {
		htmlOutput(qq("@{heatmap_id}_info"))
	}

	if(identical(output_ui, TRUE)) {
		output_ui = default_output_ui()
	} else if(identical(output_ui, FALSE)) {
		output_ui = NULL
	}

	if(is.null(output_ui)) {
		shiny_env[[heatmap_id]] = list(default_output_ui = FALSE)
	}  else {
		shiny_env[[heatmap_id]] = list(default_output_ui = identical(output_ui, default_output_ui()))
	}

	output_ui = div(
		id = qq("@{heatmap_id}_output_wrapper"),
		output_ui,
		style = qq("width: @{width3}px;")
	)

	if(layout %in% c("(1-2)|3", "12|3")) {
		layout_css = qq("
			.@{heatmap_id}_widget #@{heatmap_id}_heatmap_group {
				float:left;
			}
			.@{heatmap_id}_widget #@{heatmap_id}_sub_heatmap_group {
				float:left;
			}
		")

		tl = tagList(
			main_heatmap_ui, 
			sub_heatmap_ui,
			div(style = "clear: both;"),
			output_ui
		)
	} else if(layout %in% c("1|(2-3)", "1|23")) {
		layout_css = qq("
			.@{heatmap_id}_widget #@{heatmap_id}_sub_heatmap_group {
				float:left;
			}
			.@{heatmap_id}_widget #@{heatmap_id}_output_wrapper {
				float:left;
			}
		")

		tl = tagList(
			main_heatmap_ui, 
			sub_heatmap_ui,
			output_ui
		)
	} else if(layout %in% c("1-2-3", "123")) {
		layout_css = qq("
			.@{heatmap_id}_widget #@{heatmap_id}_heatmap_group {
				float:left;
			}
			.@{heatmap_id}_widget #@{heatmap_id}_sub_heatmap_group {
				float:left;
			}
			.@{heatmap_id}_widget #@{heatmap_id}_output_wrapper {
				float:left;
			}
		")

		tl = tagList(
			main_heatmap_ui, 
			sub_heatmap_ui,
			output_ui
		)
	} else if(layout %in% c("1|2|3")) {
		layout_css = ""
		tl = tagList(
			main_heatmap_ui, 
			sub_heatmap_ui,
			output_ui
		)
	} else if(layout %in% c("1-(2|3)")) {
		layout_css = qq("
			.@{heatmap_id}_widget #@{heatmap_id}_heatmap_group {
				float:left;
			}
		")

		tl = tagList(
			main_heatmap_ui,
			div( 
				sub_heatmap_ui,
				output_ui,
				style = "float:left;"
			)
		)
	} else {
		stop_wrap("Value of `layout` can only be one of '(1|2)-3', '1-(2|3)', '1-2-3', '1|2|3', '1|(2-3)'.")
	}

	fluidPage(class = qq("@{heatmap_id}_widget"),

		list(jqueryui_dep, pickr_dep, fontawesome_dep, ht_js_dep),

		htmlOutput(qq("@{heatmap_id}_warning")),
		tl,
		div(style = "clear: both;"),
		tags$style(HTML(layout_css)),
		...
	)
}



