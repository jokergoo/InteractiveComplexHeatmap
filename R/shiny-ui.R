
# == title
# UI for the interactive complex heatmaps
#
# == param
# -heatmap_id ID of the plot. If it is not specified, an internal ID is assigned.
# -title1 Title of the original heatmap.
# -title2 Title of the sub-heatmap.
# -title3 Title of the output.
# -width1 Width of the original heatmap.
# -height1 Height of the original heatmap.
# -width2 Width of the sub-heatmap.
# -height2 Height of the sub-heatmap.
# -width3 Width of the output div.
# -layout One of ``"(1|2)-3"``, ``"1-(2|3)"``, ``"1-2-3"``, ``"1|2|3"``, ``"1|(2-3)"``. If ``brush`` is not set with the argument ``response``, which means there is no sub-heatmap panel, the code ``2`` can be omitted.
# -compact If the value is ``TRUE``, there will be no sub-heatmap, and output floats at the mouse position when click/hover on the original heatmap.
# -action Which action for selecting single cells on the heatmap? Value should be ``click``, ``hover`` or ``dblclick``.
# -cursor When moving mouse on heatmap, whether to show the cursors on the four sides?
# -response Which action needs to be responded on the server side? Value should be in ``click``/``hover``/``dblclick``, ``brush`` and ``brush-output``.
#          ``brush`` responds in two places which are the sub-heatmap and the output components and ``brush-output`` only responds in the output component.
# -brush_opt A list of parameters passed to `shiny::brushOpts`. Do not set an ID for the brush. An internal brush ID is automatically set.
# -output_ui A `shiny::htmlOutput` or other ``*Output`` object (defined in shiny or other related packages). If it is set to ``NULL``, there is no output component in the app.
# -output_ui_float Whether the UI defined by ``output_ui`` floats at the mouse positions.
# -containment Whether the resizing is restricted in a certain parent div? Value can be ``TRUE``/``FALSE`` or a JQuery selector.
# -internal Internally used.
# -... Pass to the UI container which is wrapped by `shiny::fluidPage`.
#
# == details
# This function generates HTML fragment for the interactive UI. See the example in `makeInteractiveComplexHeatmap` page.
#
# ``layout`` is defined as follows (``1`` for the original heatmap, ``2`` for the selected sub-heatmap and ``3`` is for the output:
#
# - ``"(1-2)|3"``: Heatmap and sub-heatmap are in a same row, and output is in a second row. This is the default layout.
# - ``"1|(2-3)"``: Heatmap is in a single row, while sub-heatmap and output are in a second row.
# - ``"1-2-3"``: All three components are in a same row.
# - ``"1|2|3"``: Each component is in a single row.
# - ``"1-(2|3)"``: Being different from the other four layouts, this is a two-column layout. Heatmap is in a sigle column. Sub-heatmap and output are vertically aligned and the two are in the second column. 
#
# The hover event is implemented with https://github.com/websanova/mousestop .
#
# == value
# A UI that can be used in Shiny.
InteractiveComplexHeatmapOutput = function(heatmap_id = NULL, 
	title1 = "Original heatmap", title2 = "Selected sub-heatmap", 
	title3 = if(output_ui_float) NULL else "Output",
	width1 = ifelse(layout == "1|(2-3)", 800, 450), 
	height1 = ifelse(layout == "1-(2|3)", 700, 350), 
	width2 = 400, 
	height2 = 350, 
	width3 = NULL,
	layout = ifelse("brush" %in% response, "(1-2)|3", "1-3"), compact = FALSE,
	action = "click", cursor = TRUE,
	response = c(action, "brush"),
	brush_opt = list(stroke = "#f00", opacity = 0.6), 
	output_ui = default_output_ui(heatmap_id), 
	output_ui_float = FALSE, containment = FALSE,
	internal = FALSE,
	...) {

	if(is.null(heatmap_id)) {
		increase_widget_index()
		heatmap_id = paste0("ht", get_widget_index())
	}

	if(layout %in% c("12|3")) {
		layout = "(1-2)|3"
	} else if(layout %in% c("(1)|3", "1|(3)")) {
		layout = "1|3"
	} else if(layout %in% c("1|23")) {
		layout = "1|(2-3)"
	} else if(layout %in% c("123")) {
		layout = "1-2-3"
	} else if(layout %in% c("13")) {
		layout = "1-3"
	}

	if(is.null(width3)) {
		if(compact) {
			width3 = 400
		} else if(output_ui_float) {
			width3 = 400
		} else {
			width3 = width1
			if(layout %in% c("(1-2)|3", "12|3")) {
				width3 = width1 + width2
			} else if(layout %in% "1-(2|3)") {
				width3 = width2
			} else if(layout %in% "1|(2-3)") {
				width3 = width1 - width2
			}
		}
	}

	if(compact) {
		if(missing(response)) {
			response = c(action, "brush-output")
		} else if(any(c("brush", "brush-output") %in% response)) {
			response = setdiff(c(response, "brush-output"), "brush")
		} else { 
			response = response
		}
		main_heatmap_ui = originalHeatmapOutput(heatmap_id, title = title1, width = width1, height = height1, action = action,
			cursor = cursor, response = response, brush_opt = brush_opt, containment = containment, internal = internal)
		output_ui = HeatmapInfoOutput(heatmap_id, title = title3, width = width3, output_ui = output_ui, output_ui_float = TRUE,
			action = action, response = response, internal = internal)

		ui = fluidPage(class = qq("@{heatmap_id}_widget"),
			# htmlOutput(qq("@{heatmap_id}_warning")),
			main_heatmap_ui,
			output_ui,
			...
		)
		return(ui)
	}


	main_heatmap_ui = originalHeatmapOutput(heatmap_id, title = title1, width = width1, height = height1, action = action,
		cursor = cursor, response = response, brush_opt = brush_opt, containment = containment, internal = internal)

	sub_heatmap_ui = subHeatmapOutput(heatmap_id, title = title2, width = width2, height = height2, containment = containment, internal = internal)

	output_ui = HeatmapInfoOutput(heatmap_id, title = title3, width = width3, output_ui = output_ui, output_ui_float = output_ui_float,
		action = action, response = response, internal = internal)

	has_brush_response = "brush" %in% response
	only_brush_output_response = !(has_brush_response) & "brush-output" %in% response

	if(layout %in% c("(1-2)|3", "12|3", "1|3", "(1)|3", "1|(3)")) {
		layout_css = qq("
			.@{heatmap_id}_widget #@{heatmap_id}_heatmap_group {
				display:table-cell;
			}
			.@{heatmap_id}_widget #@{heatmap_id}_sub_heatmap_group {
				display:table-cell;
			}
		")

		tl = tagList(
			main_heatmap_ui, 
			if(has_brush_response) sub_heatmap_ui else NULL,
			output_ui
		)
	} else if(layout %in% c("1|(2-3)", "1|23")) {
		layout_css = qq("
			.@{heatmap_id}_widget #@{heatmap_id}_sub_heatmap_group {
				display:table-cell;
			}
			.@{heatmap_id}_widget #@{heatmap_id}_output_wrapper {
				display:table-cell;
			}
		")

		tl = tagList(
			main_heatmap_ui, 
			if(has_brush_response) sub_heatmap_ui else NULL,
			output_ui
		)
	} else if(layout %in% c("1-2-3", "123", "13", "1-3")) {
		layout_css = qq("
			.@{heatmap_id}_widget #@{heatmap_id}_heatmap_group {
				display:table-cell;
			}
			.@{heatmap_id}_widget #@{heatmap_id}_sub_heatmap_group {
				display:table-cell;
			}
			.@{heatmap_id}_widget #@{heatmap_id}_output_wrapper {
				display:table-cell;
			}
		")

		tl = tagList(
			main_heatmap_ui, 
			if(has_brush_response) sub_heatmap_ui else NULL,
			output_ui
		)
	} else if(layout %in% c("1|2|3", "1|3")) {
		layout_css = ""
		tl = tagList(
			main_heatmap_ui, 
			if(has_brush_response) sub_heatmap_ui else NULL,
			output_ui
		)
	} else if(layout %in% c("1-(2|3)")) {
		layout_css = qq("
			.@{heatmap_id}_widget #@{heatmap_id}_heatmap_group {
				display:table-cell;
			}
		")

		tl = tagList(
			main_heatmap_ui,
			div( 
				if(has_brush_response) sub_heatmap_ui else NULL,
				output_ui,
				style = "display:table-cell;"
			)
		)
	} else {
		stop_wrap("Value of `layout` can only be one of '(1-2)|3', '1-(2|3)', '1-2-3', '1|2|3', '1|(2-3)'.")
	}

	fluidPage(class = qq("@{heatmap_id}_widget"),
		tl,
		tags$style(HTML(layout_css)),
		...
	)
}


# == title
# UI for the original heatmap
#
# == param
# -heatmap_id ID of the plot.
# -title Title of the original heatmap.
# -width Width of the original heatmap.
# -height Height of the original heatmap.
# -action Which action for selecting single cells on the heatmap? Value should be ``click``, ``hover`` or ``dblclick``.
# -cursor When moving mouse on heatmap, whether to show the cursors on the four sides?
# -response Which action needs to be responded on the server side? Value should be in ``click``/``hover``/``dblclick``, ``brush`` and ``brush-output``.
#          ``brush`` responds in two places which are the sub-heatmap and the output components and ``brush-output`` only responds in the output component.
# -brush_opt A list of parameters passed to `shiny::brushOpts`. Do not set an ID for the brush. An internal brush ID is automatically set.
# -containment Whether the resizing is restricted in a certain parent div? Value can be ``TRUE``/``FALSE`` or a JQuery selector.
# -internal Internally used.
#
# == seealso
# `subHeatmapOutput`, `HeatmapInfoOutput`.
#
# == example
# if(interactive()) {
#     require(shinydashboard)
#     m = matrix(rnorm(100), 10)
#     ht = Heatmap(m)
#
#     body = dashboardBody(
#         fluidRow(
#             box(title = "Original heatmap", width = 4, solidHeader = TRUE, status = "primary",
#                 originalHeatmapOutput("ht")
#             ),
#             box(title = "Sub-heatmap", width = 4, solidHeader = TRUE, status = "primary",
#                 subHeatmapOutput("ht")
#             ),
#             box(title = "Output", width = 4, solidHeader = TRUE, status = "primary",
#                 HeatmapInfoOutput("ht")
#             )
#         )
#     )
#     ui = dashboardPage(
#         dashboardHeader(),
#         dashboardSidebar(),
#         body
#     )
#     server = function(input, output, session) {
#         makeInteractiveComplexHeatmap(input, output, session, ht, "ht")
#     }
#     shinyApp(ui, server)
# }
originalHeatmapOutput = function(heatmap_id, title = NULL,
	width = 450, height = 350, 
	action = "click", cursor = TRUE,
	response = c(action, "brush"),
	brush_opt = list(stroke = "#f00", opacity = 0.6),
	containment = FALSE, internal = FALSE) {

	if(missing(heatmap_id)) {
		if(length(shiny_env$heatmap) == 1) {
			heatmap_id = names(shiny_env$heatmap)
		} else if(length(shiny_env$heatmap) == 0) {
			increase_widget_index()
			heatmap_id = paste0("ht", get_widget_index())
		}
	}

	if(grepl("^\\d", heatmap_id)) {
		stop_wrap("heatmap_id cannot start with digits.")
	}

	if(is.null(shiny_env$heatmap[[heatmap_id]])) {
		shiny_env$heatmap[[heatmap_id]] = list()
		if(!internal) shiny_env$current_heatmap_id = heatmap_id
	}

	if(action %in% c("dblclick", "dbclick")) {
		click = NULL
		dblclick = qq("@{heatmap_id}_heatmap_click")
		hover = NULL
		action = "dblclick"
	} else if(action == "hover") {
		click = NULL
		dblclick = NULL
		hover = NULL
	} else if(action == "click") {
		click = qq("@{heatmap_id}_heatmap_click")
		dblclick = NULL
		hover = NULL
	} else {
		stop_wrap("`action` can only be one of `click`, `hover` and `dblclick`.")
	}
	brush = do.call(brushOpts, c(list(id = qq("@{heatmap_id}_heatmap_brush")), brush_opt))

	shiny_env$heatmap[[heatmap_id]]$action = action
	response2 = NULL
	if(any(response %in% "brush")) response2 = c(response2, "brush")
	if(any(response %in% "brush-output")) response2 = c(response2, "brush-output")

	if("click" %in% response) {
		if(!"click" %in% action) {
			stop_wrap("'click' is set with argument `response`, so 'click' must also be specified with argument `action`.")
		}
	}
	if("hover" %in% response) {
		if(!"hover" %in% action) {
			stop_wrap("'hover' is set with argument `response`, so 'hover' must also be specified with argument `action`.")
		}
	}
	if("dblclick" %in% response) {
		if(!"dblclick" %in% action) {
			stop_wrap("'dblclick' is set with argument `response`, so 'dblclick' must also be specified with argument `action`.")
		}
	}

	if(any(response %in% c("click", "hover", "dblclick"))) response2 = c(response2, "click")
	response = response2
	if(length(response) == 0) {
		stop_wrap("response can only be click/hover/dblclick + brush.")
	}
	shiny_env$heatmap[[heatmap_id]]$response = response

	has_click_reponse = "click" %in% response
	has_brush_response = "brush" %in% response
	only_brush_output_response = !(has_brush_response) & "brush-output" %in% response

	if(!has_click_reponse) {
		click = NULL
		dblclick = NULL
		hover = NULL
	}
	if(!has_brush_response & !only_brush_output_response) {
		brush = NULL
	}

	cursor = ifelse(cursor, "true", "false")

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
		version    = "5.15.3",
		package    = "fontawesome",
		src        = "fontawesome/css",
		stylesheet = c("all.min.css", "v4-shims.min.css")
    )

    clipboard_dep = htmltools::htmlDependency(
		name       = "clipboard",
		version    = "2.0.7",
		package    = "InteractiveComplexHeatmap",
		src        = "www",
		script     = c("clipboard.min.js")
    )

    mousestop_dep = htmltools::htmlDependency(
		name       = "mousestop",
		version    = "3.0.1",
		package    = "InteractiveComplexHeatmap",
		src        = "www",
		script     = c("mousestop.min.js")
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

    if(identical(containment, FALSE)) {
		containment = "false"
	} else if(identical(containment, TRUE)) {
		containment = qq("$('#@{heatmap_id}_heatmap_group').parent()")
	} else {
		containment = paste0('"', containment, '"')
	}

	layout = shiny_env$heatmap[[heatmap_id]]$layout
	if(is.null(layout)) layout = ""

	heatmap_hash = paste0("c", digest(heatmap_id, "crc32"))
    main_heatmap_ui = div(id = qq("@{heatmap_id}_heatmap_group"),

    	list(jqueryui_dep, 
    		 pickr_dep, 
    		 clipboard_dep, 
    		 fontawesome_dep, 
    		 mousestop_dep,
    		 add_js_css_dep(heatmap_id, js_file = "ht-main.js", css_file = "ht-main.css")
    	),

    	# htmlOutput(qq("@{heatmap_id}_warning")),

		if(identical(title, NULL) || identical(title, "")) NULL else h5(title),

		div(id = qq("@{heatmap_id}_heatmap_resize"),
			plotOutput(qq("@{heatmap_id}_heatmap"), height = height, width = width,
				        brush = brush,
				        click = click, dblclick = dblclick, hover = hover
			),
			tags$script(HTML(qq("
				$('#@{heatmap_id}_heatmap').html('<p style=\"position:relative;top:50%;\">Making heatmap, please wait...</p>');
			")))
		),
		tags$script(HTML(qq('
			$("#@{heatmap_id}_heatmap_resize").css("width", $("#@{heatmap_id}_heatmap").width() + 4);
			$("#@{heatmap_id}_heatmap_resize").css("height", $("#@{heatmap_id}_heatmap").height() + 4);
		'))),
		div(id = qq("@{heatmap_id}_heatmap_control"),
			style = "display:none;",
			{
				tbl = list(
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
							numericInput(qq("@{heatmap_id}_heatmap_download_image_width"), label = "Image width (in px)", value = 0),
							numericInput(qq("@{heatmap_id}_heatmap_download_image_height"), label = "Image height (in px)", value = 0),
							downloadButton(qq("@{heatmap_id}_heatmap_download_button"), "Save image")
						)
					),
					tabPanel(HTML("<i class='fa fa-expand-arrows-alt'></i>"),
						div(id = qq('@{heatmap_id}_tabs-resize'),
							numericInput(qq("@{heatmap_id}_heatmap_input_width"), "Box width", 0),
							numericInput(qq("@{heatmap_id}_heatmap_input_height"), "Box height", 0),
							actionButton(qq("@{heatmap_id}_heatmap_input_size_button"), "Change image size")
						)
					)
				)
				if(!has_brush_response & !only_brush_output_response) { # only click
					tbl = tbl[-(1:2)]
				} else if(!has_brush_response & only_brush_output_response) { # no brush (subheatmap) output, only brush (text) output + click output
					tbl = tbl[-1]
				}
				do.call(tabsetPanel, tbl)
			},
			tags$script(HTML(qq("
				$('#@{heatmap_id}_heatmap_download_image_width').val($('#@{heatmap_id}_heatmap').width());
				$('#@{heatmap_id}_heatmap_download_image_height').val($('#@{heatmap_id}_heatmap').height());
				$('#@{heatmap_id}_heatmap_input_width').val($('#@{heatmap_id}_heatmap').width());
				$('#@{heatmap_id}_heatmap_input_height').val($('#@{heatmap_id}_heatmap').height());
			")))
		)	
	)
	main_heatmap_ui
}

# == title
# UI for the sub-heatmaps
#
# == param
# -heatmap_id ID of the plot.
# -title Title of the sub-heatmap.
# -width Width of the sub-heatmap.
# -height Height of the sub-heatmap.
# -containment Whether the resizing is restricted in a certain parent div? Value can be ``TRUE``/``FALSE`` or a JQuery selector.
# -internal Internally used.
#
# == seealso
# `originalHeatmapOutput`.
#
# == example
# # See examples on the help page of originalHeatmapOutput()
subHeatmapOutput = function(heatmap_id, title = NULL,
	width = 400, height = 350, containment = FALSE, internal = FALSE) {

	if(missing(heatmap_id)) {
		if(length(shiny_env$heatmap) == 1) {
			heatmap_id = names(shiny_env$heatmap)
		} else if(length(shiny_env$heatmap) == 0) {
			increase_widget_index()
			heatmap_id = paste0("ht", get_widget_index())
		}
	}
	if(is.null(shiny_env$heatmap[[heatmap_id]])) {
		shiny_env$heatmap[[heatmap_id]] = list()
		if(!internal) shiny_env$current_heatmap_id = heatmap_id
	}

	if(identical(containment, FALSE)) {
		containment = "false"
	} else if(identical(containment, TRUE)) {
		containment = qq("$('#@{heatmap_id}_sub_heatmap_group').parent()")
	} else {
		containment = paste0('"', containment, '"')
	}

	layout = shiny_env$heatmap[[heatmap_id]]$layout
	if(is.null(layout)) layout = ""

	heatmap_hash = paste0("c", digest(heatmap_id, "crc32"))
	sub_heatmap_ui = div(
		id = qq("@{heatmap_id}_sub_heatmap_group"),
		
		add_js_css_dep(heatmap_id, js_file = "ht-sub.js", css_file = "ht-sub.css"),
		
		if(identical(title, NULL) || identical(title, "")) NULL else h5(title),
		div(id = qq("@{heatmap_id}_sub_heatmap_resize"),
			plotOutput(qq("@{heatmap_id}_sub_heatmap"), height = height, width = width)
		),
		tags$script(HTML(qq('
			$("#@{heatmap_id}_sub_heatmap_resize").css("width", $("#@{heatmap_id}_sub_heatmap").width() + 4);
			$("#@{heatmap_id}_sub_heatmap_resize").css("height", $("#@{heatmap_id}_sub_heatmap").height() + 4);
		'))),
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
							checkboxInput(qq("@{heatmap_id}_remove_empty_checkbox"), label = "Remove empty rows and columns", value = FALSE),
							HTML(qq('
						<p style="padding-top:4px;">
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
						numericInput(qq("@{heatmap_id}_sub_heatmap_download_image_width"), label = "Image width (in px)", value = 0),
						numericInput(qq("@{heatmap_id}_sub_heatmap_download_image_height"), label = "Image height (in px)", value = 0),
						downloadButton(qq("@{heatmap_id}_sub_heatmap_download_button"), "Save image")
					),
				),
				tabPanel(HTML("<i class='fa fa-expand-arrows-alt'></i>"),
					div(id = qq('@{heatmap_id}_sub_tabs-resize'),
						numericInput(qq("@{heatmap_id}_sub_heatmap_input_width"), "Box width", 0),
						numericInput(qq("@{heatmap_id}_sub_heatmap_input_height"), "Box height", 0),
						actionButton(qq("@{heatmap_id}_sub_heatmap_input_size_button"), "Change image size")
					)
				)
			),
			tags$script(HTML(qq("
				$('#@{heatmap_id}_sub_heatmap_download_image_width').val($('#@{heatmap_id}_sub_heatmap').width());
				$('#@{heatmap_id}_sub_heatmap_download_image_height').val($('#@{heatmap_id}_sub_heatmap').height());
				$('#@{heatmap_id}_sub_heatmap_input_width').val($('#@{heatmap_id}_sub_heatmap').width());
				$('#@{heatmap_id}_sub_heatmap_input_height').val($('#@{heatmap_id}_sub_heatmap').height());
			")))
		),
		style = qq("width:@{width}; height:@{height};")
	)

	sub_heatmap_ui
}

default_output_ui = function(heatmap_id) {
	htmlOutput(paste0(heatmap_id, "_info"))
}


# == title
# UI for the output
#
# == param
# -heatmap_id ID of the plot.
# -title Title of the output.
# -width Width of the output div.
# -output_ui A `shiny::htmlOutput` or other ``*Output`` object (defined in shiny or other related packages).
# -output_ui_float Whether the UI defined by ``output_ui`` floats at the mouse positions.
# -action It is only used when ``output_ui_float = TRUE`` to properly bind the floating frame to the event on heatmap (i.e. ``click``, ``hover`` or ``dblclick``).
#      If `HeatmapInfoOutput` is executed after `originalHeatmapOutput`, the value for it is automatically decided
# -response It is only used when ``output_ui_float = TRUE`` and ``response = "brush"`` or ``response = "brush-output"``, so that single clicking or hovering won't have any effect, in other word, there is only response from brushing.
#       If `HeatmapInfoOutput` is executed after `originalHeatmapOutput`, the value for it is automatically decided
# -internal Internally used.
#
# == seealso
# `originalHeatmapOutput`, `subHeatmapOutput`.
#
# == example
# # See examples on the help page of originalHeatmapOutput()
HeatmapInfoOutput = function(heatmap_id, title = NULL, width = 400, 
	output_ui = default_output_ui(heatmap_id), 
	output_ui_float = FALSE, action = NULL, response = NULL, internal = FALSE) {

	if(missing(heatmap_id)) {
		if(length(shiny_env$heatmap) == 1) {
			heatmap_id = names(shiny_env$heatmap)
		} else if(length(shiny_env$heatmap) == 0) {
			increase_widget_index()
			heatmap_id = paste0("ht", get_widget_index())
		}
	}

	if(is.null(shiny_env$heatmap[[heatmap_id]])) {
		shiny_env$heatmap[[heatmap_id]] = list()
		if(!internal) shiny_env$current_heatmap_id = heatmap_id
	}

	if(identical(output_ui, TRUE)) {
		output_ui = default_output_ui(heatmap_id)
	} else if(identical(output_ui, FALSE)) {
		output_ui = NULL
	}

	shiny_env$heatmap[[heatmap_id]]$output_ui_float = output_ui_float

	if(is.null(output_ui)) {
		shiny_env$heatmap[[heatmap_id]]$default_output_ui = FALSE
	}  else {
		shiny_env$heatmap[[heatmap_id]]$default_output_ui = identical(output_ui, default_output_ui(heatmap_id))
	}

	default_output_ui_float = output_ui_float & shiny_env$heatmap[[heatmap_id]]$default_output_ui

	if(is.null(response)) {
		if(is.null(shiny_env$heatmap[[heatmap_id]]$response)) {
			response = c("click", "brush")
		} else {
			response = shiny_env$heatmap[[heatmap_id]]$response
		}
	}

	if(is.null(action)) {
		if(is.null(shiny_env$heatmap[[heatmap_id]]$action)) {
			if(identical(response, "brush") || identical(response, "brush-output")) {
				action = ""
			} else {
				action = "click"
			}
		} else {
			action = shiny_env$heatmap[[heatmap_id]]$action
		}
	} else {
		if(identical(response, "brush") || identical(response, "brush-output")) {
			action = ""
		}
	}

	if(is.numeric(width)) {
		if(width > 1) {
			width = paste0(width, "px")
		} else {
			width = paste0(width*100, "%")
		}
	}

	heatmap_hash = paste0("c", digest(heatmap_id, "crc32"))
    div(
		id = qq("@{heatmap_id}_output_wrapper"),
		add_js_css_dep(heatmap_id, js_file = "ht-output.js", css_file = "ht-output.css"),
		if(identical(title, NULL) || identical(title, "")) NULL else h5(title),
		output_ui,
		style = qq("width: @{width}"),
		if(output_ui_float) tags$script(HTML(qq("$(document.body).append( $('#@{heatmap_id}_output_wrapper').detach() );"))) else NULL
	)
}

add_js_css_dep = function(heatmap_id, js_file, css_file, envir = parent.frame()) {

	if(identical(topenv(), .GlobalEnv)) {
    	ht_js = paste(readLines(qq("~/project/development/InteractiveComplexHeatmap/inst/template/@{js_file}")), collapse = "\n")
    	version = "0.0.1"
    } else {
    	ht_js = paste(readLines(system.file("template", js_file, package = "InteractiveComplexHeatmap")), collapse = "\n")
    	version = packageVersion("InteractiveComplexHeatmap")
    }
    temp_js = qq(ht_js, envir = envir)
    temp_js = paste0("<script>\n", temp_js, "\n</script>\n")

    if(identical(topenv(), .GlobalEnv)) {
    	ht_css = paste(readLines(qq("~/project/development/InteractiveComplexHeatmap/inst/template/@{css_file}")), collapse = "\n")
    } else {
		ht_css = paste(readLines(system.file("template", css_file, package = "InteractiveComplexHeatmap")), collapse = "\n")
    }
    temp_css = qq(ht_css, envir = envir)
    temp_css = paste0("<style>\n", temp_css, "\n</style>")

    htmltools::htmlDependency(
		name   = qq("interactive-complex-heatmap-@{heatmap_id}-@{gsub('.(js|css)$', '', basename(js_file))}"),
		version = version,
		package    = "InteractiveComplexHeatmap",
		src        = "www",
		script     = "foo.js",
		head = paste0(temp_js, "\n", temp_css)
    )
}
