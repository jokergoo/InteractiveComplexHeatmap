
shiny_env = new.env()
shiny_env$history = list()

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
# -nrow Should the two heatmap ``div`` be put in one row or in two rows? Value should be either 1 or 2. 
# -action Which action for selecting the cell on the heatmap? Value should be ``click``, ``hover`` or ``dblclick``.
# -brush_opt A list of parameters passed to `shiny::brushOpts`.
# -output_div Whether to add the output ``div``
# -css Self-defined CSS code.
#
# == details
# This function generates HTML fragment for the interactive UI. See the example from `renderInteractiveComplexHeatmap` page.
#
# It generates three div blocks. Assuming the heatmap id variable is ``heatmap_id``, the three div blocks are:
#
# - ``#{heatmap_id}_heatmap_wrap_outer``: to put the original heatmap. This div contains two children elements. One is the title
#    for the heatmap (with a ``h3`` tag) and one is a div block with ID ``#{heatmap_id}_heatmap_wrap``. ``#{heatmap_id}_heatmap_wrap``
#    is for JQuery-UI and it wraps the div ``#{heatmap_id}_heatmap`` which is used by `shiny::plotOutput`.
# - ``#{heatmap_id}_sub_heatmap_wrap_outer``: to put the sub-heatmap. This div contains two children elements. One is the title
#    for the heatmap (with a ``h3`` tag) and one is a div block with ID ``#{heatmap_id}_sub_heatmap_wrap``. ``#{heatmap_id}_sub_heatmap_wrap``
#    is for JQuery-UI and it wraps the div ``#{heatmap_id}_sub_heatmap`` which is used by `shiny::plotOutput`.
# - ``#{heatmap_id}_info``: to put the information of the selected position/area.
#
# == value
# A UI that can be used in shiny.
InteractiveComplexHeatmapOutput = function(heatmap_id = NULL, 
	title1 = "Original heatmap", title2 = "Selected sub-heatmap",
	width1 = 400, height1 = 350, width2 = 370, height2 = 350, nrow = 1,
	action = c("click", "hover", "dblclick"), brush_opt = list(), 
	output_div = TRUE, css = "") {

	if(is.null(heatmap_id)) {
		heatmap_id = paste0("hash_", digest(Sys.time()))
		shiny_env$current_heatmap_id = heatmap_id
	}

	if(grepl("^\\d", heatmap_id)) {
		stop_wrap("heatmap_id cannot start with digits.")
	}

	shiny_env[[heatmap_id]] = list()

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

	fluidPage(

		tags$script(HTML(paste(readLines(system.file("app", "jquery-ui.js", package = "InteractiveComplexHeatmap"), warn = FALSE), collapse = "\n"))),

		tags$script(HTML(qq(
'$( function() {
   $("#@{heatmap_id}_heatmap_wrap").resizable({
	 stop: function( event, ui ) {
    	document.getElementById("@{heatmap_id}_mask").remove();
    	$("#@{heatmap_id}_heatmap_brush").remove();
    	$("#@{heatmap_id}_heatmap").height(ui.size.height-4);
    	$("#@{heatmap_id}_heatmap").width(ui.size.width-4);
     },
     start: function(event, ui) {
     	var mask = document.createElement("div");
     	mask.setAttribute("style", "position:absolute;top:0;background-color:rgba(255, 255, 0, 0.5)");
     	mask.setAttribute("id", "@{heatmap_id}_mask");
     	$("#@{heatmap_id}_heatmap_wrap").append(mask);
     },
     resize: function(event, ui) {
     	$("#@{heatmap_id}_mask").width(ui.size.width);
     	$("#@{heatmap_id}_mask").height(ui.size.height);
     }
   });

   $("#@{heatmap_id}_sub_heatmap_wrap").resizable({
	 stop: function( event, ui ) {
    	document.getElementById("@{heatmap_id}_mask2").remove();
    	$("#@{heatmap_id}_sub_heatmap").height(ui.size.height-4);
    	$("#@{heatmap_id}_sub_heatmap").width(ui.size.width-4);
     },
     start: function(event, ui) {
     	var mask = document.createElement("div");
     	mask.setAttribute("style", "position:absolute;top:0;background-color:rgba(255, 255, 0, 0.5)");
     	mask.setAttribute("id", "@{heatmap_id}_mask2");
     	$("#@{heatmap_id}_sub_heatmap_wrap").append(mask);
     },
     resize: function(event, ui) {
     	$("#@{heatmap_id}_mask2").width(ui.size.width);
     	$("#@{heatmap_id}_mask2").height(ui.size.height);
     }
   });
});
'))),
		tags$style(paste(readLines(system.file("app", "jquery-ui.css", package = "InteractiveComplexHeatmap")), collapse = "\n")),
		tags$style(qq("
#@{heatmap_id}_heatmap_wrap_outer, #@{heatmap_id}_sub_heatmap_wrap_outer {
	@{ifelse(nrow == 1, 'float:left;', '')}
	margin-bottom: 10px;
}
#@{heatmap_id}_heatmap_wrap_outer {
	margin-right: 10px;
}
#@{heatmap_id}_heatmap_wrap {
	width: @{width1}px;
	height: @{height1}px;
	position:relative;
	border:1px solid grey;
	text-align:center;
}
#@{heatmap_id}_sub_heatmap_wrap {
	width: @{width2}px;
	height: @{height2}px;
	position: relative;
	border: 1px solid grey;
}
#@{heatmap_id}_heatmap, #@{heatmap_id}_sub_heatmap {
	display: block;
    margin: auto;
    margin: auto;
}
#@{heatmap_id}_sub_heatmap_wrap_outer .checkbox {
	padding:2px 0px;
	margin:0px
}
#@{heatmap_id}_sub_heatmap_wrap_outer .form-group {
	padding: 0px;
	margin: 0px;
}
@{css}
")),
	div(
		h5(title1),
		div(
			plotOutput(qq("@{heatmap_id}_heatmap"), height = height1 - 4, width = width1 - 4,
				        brush = do.call(brushOpts, c(list(id = qq("@{heatmap_id}_heatmap_brush")), brush_opt)),
				        click = click, dblclick = dblclick, hover = hover
			),
			tags$script(HTML(qq("
				$('#@{heatmap_id}_heatmap').html('<p style=\"position:relative;top:50%;\">Making heatmap, please wait...</p>');
			"))),
			id = qq("@{heatmap_id}_heatmap_wrap")
		),
		htmlOutput(qq("@{heatmap_id}_search")),
		id = qq("@{heatmap_id}_heatmap_wrap_outer")
	),
	div(
		h5(title2),
		div(
			plotOutput(qq("@{heatmap_id}_sub_heatmap"), height = height2 - 4, width = width2 - 4),
			id = qq("@{heatmap_id}_sub_heatmap_wrap")
		),
		div(
			div(
				div(checkboxInput(qq("@{heatmap_id}_show_row_names_checkbox"), label = "Show row names", value = TRUE), style = "float:left;width:170px"),
				div(checkboxInput(qq("@{heatmap_id}_show_column_names_checkbox"), label = "Show column names", value = TRUE), style = "float:left;width:170px"),
				div(style = "clear: both;")
			),
			div(
				checkboxInput(qq("@{heatmap_id}_show_annotation_checkbox"), label = "Show heatmap annotations", value = TRUE),
				checkboxInput(qq("@{heatmap_id}_show_cell_fun_checkbox"), label = "Show cell decorations", value = FALSE),
				actionLink(qq("@{heatmap_id}_show_table", "Open tables"))
			)
		),
		id = qq("@{heatmap_id}_sub_heatmap_wrap_outer")
	),
	div(style = "clear: both;"),
	if(output_div) htmlOutput(qq("@{heatmap_id}_info")) else NULL
	)
}

# == title
# Process the heatmaps on the sever side
#
# == param
# -ht_list A `ComplexHeatmap::Heatmap-class` or a `ComplexHeatmap::HeatmapList-class` object.
# -input Passed from the shiny server function.
# -output Passed from the shiny server function.
# -session Passed from the shiny server function.
# -heatmap_id The corresponding heatmap ID from the UI. If there is only one interactive heatmap in the app, 
#     this argument does not need to be specified and it will use the current one specified in `InteractiveComplexHeatmapOutput`.
# -click_action Additional action at the sever side when receiving a click event on the UI. If ``action`` is selected as ``hover``
#        or ``dblclick`` in `InteractiveComplexHeatmapOutput`, then this argument controls the action for the hover or dblclick event.
# -brush_action Additional action at the sever side when receiving a brush event on the UI.
# -default_click_action Whether to apply the default click action on the sever side.
# -default_brush_action Whether to apply the default brush action on the sever side. There are two default brush actions on the server side.
#       One is to draw the sub-heatmap, and the second is to print text messages. This argument only controls the second default brush action.
#
# == value
# No value is returned.
#
# == examples
# if(interactive()) {
# ht = Heatmap(m)
# ht = draw(ht)
#
# ui = fluidPage(
#     InteractiveComplexHeatmapOutput()
# )
#
# server = function(input, output, session) {
#     renderInteractiveComplexHeatmap(ht, input, output, session)
# }
#
# shiny::shinyApp(ui, server)
# }
renderInteractiveComplexHeatmap = function(ht_list, input, output, session, 
	heatmap_id = shiny_env$current_heatmap_id,
	click_action = NULL, brush_action = NULL, 
	default_click_action = TRUE, default_brush_action = TRUE) {

	if(inherits(ht_list, "Heatmap")) {
		message("The heatmap is suggested to be updated by e.g. `ht = draw(ht)` before sending to the Shiny app.")
	} else if(inherits(ht_list, "HeatmapList")) {
		if(!ht_list@layout$initialized) {
			message("The heatmap list is suggested to be udpated by e.g. `ht_list = draw(ht_list)` before sending to the Shiny app.")
		}
	} else {
		stop_wrap("`ht_list` can only be a Heatmap/heatmapList object.")
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
		stop_wrap("There should be at least one normal heatmap (nrow > 0 and ncol > 0) in the heatmap list.")
	}

	shiny_env[[heatmap_id]] = list()

	observeEvent(
		session$clientData[[qq("output_@{heatmap_id}_heatmap_width")]] || 
		session$clientData[[qq("output_@{heatmap_id}_heatmap_height")]], {

		output[[qq("@{heatmap_id}_heatmap")]] = renderPlot({
			width = session$clientData[[qq("output_@{heatmap_id}_heatmap_width")]]
	    	height = session$clientData[[qq("output_@{heatmap_id}_heatmap_height")]]
	    	
	    	showNotification("Making the original heatmap.", duration = 2, type = "message")

	    	if(!is.null(shiny_env[[heatmap_id]]$ht_list)) {
	    		draw(shiny_env[[heatmap_id]]$ht_list)
	    	} else {
		    	if(inherits(ht_list, "Heatmap")) {
		    		shiny_env[[heatmap_id]]$ht_list = draw(ht_list)
		    	} else {
		    		if(ht_list@layout$initialized) {
		    			shiny_env[[heatmap_id]]$ht_list = do.call(draw, c(list(object = ht_list), ht_list@ht_list_param$called_arguments))
		    		} else {
		    			shiny_env[[heatmap_id]]$ht_list = draw(ht_list)
		    		}
		    	}
		    }
			shiny_env[[heatmap_id]]$ht_pos = ht_pos_on_device(shiny_env[[heatmap_id]]$ht_list, include_annotation = TRUE, calibrate = FALSE)

			shiny_env[[heatmap_id]]$selected = NULL

			if(default_click_action || default_brush_action) {
				output[[qq("@{heatmap_id}_info")]] = renderUI({
					HTML("<p>No position is selected.</p>")
				})
			}

			for(i in seq_along(shiny_env[[heatmap_id]]$ht_list@ht_list)) {
				if(inherits(shiny_env[[heatmap_id]]$ht_list@ht_list[[i]], "Heatmap")) {
					if(!is.null(shiny_env[[heatmap_id]]$ht_list@ht_list[[i]]@matrix_param$cell_fun)) {
						updateCheckboxInput(session, qq("@{heatmap_id}_show_cell_fun_checkbox"), value = TRUE)
						break
					}
					if(!is.null(shiny_env[[heatmap_id]]$ht_list@ht_list[[i]]@matrix_param$layer_fun)) {
						updateCheckboxInput(session, qq("@{heatmap_id}_show_cell_fun_checkbox"), value = TRUE)
						break
					}
				}
			}

			message(qq("[@{Sys.time()}] make the original heatmap and calculate positions (device size: @{width}x@{height} px)."))
		})

		output[[qq("@{heatmap_id}_search")]] = renderUI({
			all_ht_name = sapply(shiny_env[[heatmap_id]]$ht_list@ht_list, function(x) {
				if(inherits(x, "Heatmap")) x@name else NA
			})
			all_ht_name = all_ht_name[!is.na(all_ht_name)]

			has_row_labels = sapply(shiny_env[[heatmap_id]]$ht_list@ht_list, function(x) {
				if(inherits(x, "Heatmap")) {
					!is.null(x@row_names_param$labels)
				} else {
					FALSE
				}
			})
			has_row_labels = has_row_labels[all_ht_name]
			has_column_labels = sapply(shiny_env[[heatmap_id]]$ht_list@ht_list, function(x) {
				if(inherits(x, "Heatmap")) {
					!is.null(x@column_names_param$labels)
				} else {
					FALSE
				}
			})
			has_column_labels = has_column_labels[all_ht_name]
			if(!any(has_row_labels) && !any(has_column_labels)) {
				return(NULL)
			}
			if(any(has_row_labels) && any(has_column_labels)) {
				if(length(all_ht_name) == 1 && has_row_labels[1] && has_column_labels[1]) {
					where_choices = list("on rows" = 1, "on columns" = 2, "both" = 3)
				} else {
					where_choices = list("on rows" = 1, "on columns" = 2)
				}
			} else if(!any(has_row_labels)) {
				where_choices = list("on columns" = 2)
			} else if(!any(has_column_labels)) {
				where_choices = list("on rows" = 1)
			}

			heatmaps_to_search = all_ht_name[has_row_labels | has_column_labels]
			html = div(
				div(textInput(qq("@{heatmap_id}_keyword"), placeholder = "Multiple keywords separated by ','", label = ""), style = "width:250px;float:left;"),
				div(checkboxInput(qq("@{heatmap_id}_search_regexpr"), label = "Regular expression", value = FALSE), style = "width:150px;float:left;padding-top:15px;padding-left:4px;"),
				div(style = "clear: both;"),
				radioButtons(qq("@{heatmap_id}_search_where"), label = "Which dimension to search?", choices = where_choices, selected = where_choices[[1]], inline = TRUE),
				if(length(heatmaps_to_search) > 0) {
					checkboxGroupInput(qq("@{heatmap_id}_search_heatmaps"), label = "Which heatmaps to search?", choiceNames = unname(heatmaps_to_search), choiceValues = unname(heatmaps_to_search), selected = unname(heatmaps_to_search))
				} else {
					NULL
				},
				checkboxGroupInput(qq("@{heatmap_id}_search_extend"), label = "Extend to all heatmaps and annotations?", choiceNames = "yes", choiceValues = 1, selected = NULL),
				actionButton(qq("@{heatmap_id}_search_action"), label = "Search"),
				tags$script(HTML(qq("
					$('#@{heatmap_id}_keyword').click(function() {$('#@{heatmap_id}_heatmap_brush').remove();});
					$('#@{heatmap_id}_search_regexpr').change(function() {
						if(this.checked) {
							$('#@{heatmap_id}_keyword').attr('placeholder', 'A single regular expression');
						} else {
							$('#@{heatmap_id}_keyword').attr('placeholder', \"Multiple keywords separated by ','\");
						}
					})
				")))
			)
			HTML(qq("<details><summary style='font-weight: 500;padding:5px 0px;'>Search heatmaps</summary>@{as.character(html)}</details>"))
		})
	})

	# default
	output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
		grid.newpage()
		grid.text("No area on the heatmap is selected.", 0.5, 0.5)

		message(qq("[@{Sys.time()}] no area on the heatmap is selected, Do not make the sub-heatmap."))
	})

	if(default_click_action || default_brush_action) {
		output[[qq("@{heatmap_id}_info")]] = renderUI({
			HTML("<p>No position is selected.</p>")
		})
	}

	observeEvent(input[[qq("@{heatmap_id}_heatmap_brush")]], {

		if(is.null(input[[qq("@{heatmap_id}_heatmap_brush")]])) {
			shiny_env[[heatmap_id]]$selected = NULL
		} else {
			lt = get_pos_from_brush(input[[qq("@{heatmap_id}_heatmap_brush")]])
		  	pos1 = lt[[1]]
		  	pos2 = lt[[2]]
		    
		    ht_list = shiny_env[[heatmap_id]]$ht_list
		    dev.null()
		    selected = selectArea(ht_list, mark = FALSE, pos1 = pos1, pos2 = pos2, verbose = FALSE, ht_pos = shiny_env[[heatmap_id]]$ht_pos, include_annotation = TRUE, calibrate = FALSE)
		    dev.off2()
		    shiny_env[[heatmap_id]]$selected = selected
		}

		updateTextInput(session, qq("@{heatmap_id}_keyword"), value = "")

		output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
			
    		if(is.null(shiny_env[[heatmap_id]]$selected)) {
    			grid.newpage()
				grid.text("No area on the heatmap is selected.", 0.5, 0.5)
    		} else {
    			make_sub_heatmap(input, output, session, heatmap_id)
			}
		})
	
		if(default_brush_action) {
			default_brush_action(input, output, session, heatmap_id)
		}

		if(!is.null(brush_action)) {
			brush_action(shiny_env[[heatmap_id]]$selected, output)
		}
	})

	observeEvent(input[[qq("@{heatmap_id}_search_action")]], {
		if(input[[qq("@{heatmap_id}_keyword")]] == "") {
			output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
				grid.newpage()
				grid.text("Query keyword is empty.", 0.5, 0.5)
			})
			return(invisible(NULL))
		}

		keywords2 = keywords = input[[qq("@{heatmap_id}_keyword")]]

		where = input[[qq("@{heatmap_id}_search_where")]]
		is_regexpr = input[[qq("@{heatmap_id}_search_regexpr")]]
		sht = input[[qq("@{heatmap_id}_search_heatmaps")]]
		extend = input[[qq("@{heatmap_id}_search_extend")]]

		if(length(sht) == 0) {
			output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
				grid.newpage()
				grid.text("No heatmap is selected for searching.", 0.5, 0.5)
			})
			return(invisible(NULL))
		}

		ht_list = shiny_env[[heatmap_id]]$ht_list

		all_ht_name = sapply(ht_list@ht_list, function(x) {
			if(inherits(x, "Heatmap")) x@name else NA
		})
		all_ht_name = all_ht_name[!is.na(all_ht_name)]

		message(qq("[@{Sys.time()}] search heatmap @{ifelse(where == 1, 'row', 'column')}s with @{ifelse(is_regexpr, 'regular expression', 'keywords')}: '@{keywords}'."))

		if(!is_regexpr) {
			keywords = gsub("^\\s+||\\s+$", "", keywords)
			keywords = strsplit(keywords, "\\s*,\\s*")[[1]]
		}

		if(where == 1) {
			selected = selectByLabels(ht_list, row_keyword = keywords, keyword_is_regexpr = is_regexpr, include_annotation = TRUE, heatmap = sht, all = length(extend))
		} else if(where == 2) {
			selected = selectByLabels(ht_list, column_keyword = keywords, keyword_is_regexpr = is_regexpr, include_annotation = TRUE, heatmap = sht, all = length(extend))
		} else {
			selected = selectByLabels(ht_list, row_keyword = keywords, column_keyword = keywords, keyword_is_regexpr = is_regexpr, include_annotation = TRUE, heatmap = sht, all = length(extend))
		}
		shiny_env[[heatmap_id]]$selected = selected

		output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
			
    		if(is.null(shiny_env[[heatmap_id]]$selected)) {
    			grid.newpage()
				grid.text(paste(strwrap(qq("Found nothing from heatmaps with keywords '@{keywords2}'."), width = 60), collapse = "\n"), 0.5, 0.5)
    		} else {
    			make_sub_heatmap(input, output, session, heatmap_id)
			}
		})

		if(default_brush_action) {
			default_brush_action(input, output, session, heatmap_id)
		}

		if(!is.null(brush_action)) {
			brush_action(shiny_env[[heatmap_id]]$selected, output)
		}
	
	})

	observeEvent(input[[qq("@{heatmap_id}_heatmap_click")]], {
		
		pos1 = get_pos_from_click(input[[qq("@{heatmap_id}_heatmap_click")]])
		  
		if(is.null(pos1)) {
			shiny_env[[heatmap_id]]$selected = NULL
		} else {
			ht_list = shiny_env[[heatmap_id]]$ht_list
			dev.null()
			pos = selectPosition(ht_list, mark = FALSE, pos = pos1, verbose = FALSE, ht_pos = shiny_env[[heatmap_id]]$ht_pos, calibrate = FALSE)
			dev.off2()
			shiny_env[[heatmap_id]]$selected = pos
		}

		if(default_click_action) {
			default_click_action(input, output, session, heatmap_id)
		}

		if(!is.null(click_action)) {
			click_action(shiny_env[[heatmap_id]]$selected, output)
		}
	})

	observeEvent(input[[qq("@{heatmap_id}_search_heatmaps")]], {
		selected_ht_names = input[[qq("@{heatmap_id}_search_heatmaps")]]

		has_row_labels = sapply(shiny_env[[heatmap_id]]$ht_list@ht_list, function(x) {
			if(inherits(x, "Heatmap")) {
				!is.null(x@row_names_param$labels)
			} else {
				FALSE
			}
		})
		has_row_labels = has_row_labels[selected_ht_names]
		has_column_labels = sapply(shiny_env[[heatmap_id]]$ht_list@ht_list, function(x) {
			if(inherits(x, "Heatmap")) {
				!is.null(x@column_names_param$labels)
			} else {
				FALSE
			}
		})
		has_column_labels = has_column_labels[selected_ht_names]
		

		if(any(has_row_labels) && any(has_column_labels)) {
			if(length(selected_ht_names) == 1 && has_row_labels[1] && has_column_labels[1]) {
				where_choices = list("on rows" = 1, "on columns" = 2, "both" = 3)
			} else {
				where_choices = list("on rows" = 1, "on columns" = 2)
			}
		} else if(!any(has_row_labels)) {
			where_choices = list("on columns" = 2)
		} else if(!any(has_column_labels)) {
			where_choices = list("on rows" = 1)
		}

		updateRadioButtons(session, qq("@{heatmap_id}_search_where"), label = "Which dimension to search", choices = where_choices, selected = where_choices[[1]], inline = TRUE)

	})
}


get_pos_from_brush = function(brush) {
	coords = brush$coords_css
	if(is.null(coords)) return(NULL)
    height = (brush$range$bottom - brush$range$top)/brush$img_css_ratio$y
    pos1 = unit(c(coords$xmin, height - coords$ymin), "pt")
    pos2 = unit(c(coords$xmax, height - coords$ymax), "pt")

    list(pos1, pos2)
}

get_pos_from_click = function(click) {
	coords = click$coords_css
	if(is.null(coords)) return(NULL)
	height = (click$range$bottom - click$range$top)/click$img_css_ratio$y
    pos1 = unit(c(coords$x, height - coords$y), "pt")
    pos1
}

make_sub_heatmap = function(input, output, session, heatmap_id) {
	showNotification("Making the selected sub-heatmap.", duration = 2, type = "message")

	width = session$clientData[[qq("output_@{heatmap_id}_sub_heatmap_width")]]
    height = session$clientData[[qq("output_@{heatmap_id}_sub_heatmap_height")]]

	show_row_names = input[[qq("@{heatmap_id}_show_row_names_checkbox")]]
	show_column_names = input[[qq("@{heatmap_id}_show_column_names_checkbox")]]
	show_annotation = input[[qq("@{heatmap_id}_show_annotation_checkbox")]]
	show_cell_fun = input[[qq("@{heatmap_id}_show_cell_fun_checkbox")]]

	selected = shiny_env[[heatmap_id]]$selected
    if(is.null(selected)) {
    	grid.newpage()
		grid.text("Selected area should overlap to heatmap bodies", 0.5, 0.5)
    } else {

    	all_ht_name = unique(selected$heatmap)

    	ht_list = shiny_env[[heatmap_id]]$ht_list

    	ht_select = NULL
		for(ht_name in all_ht_name) {
			ht_current_full = ht_list@ht_list[[ht_name]]

			if(inherits(ht_current_full, "Heatmap")) {
    			selected_current = selected[selected$heatmap == ht_name, ]
    			l1 = !duplicated(selected_current$row_slice)
    			rlt = selected_current$row_index[l1]
    			l2 = !duplicated(selected_current$column_slice)
    			clt = selected_current$column_index[l2]

    			ri = unlist(rlt)
    			ci = unlist(clt)
    			rs = rep(seq_along(rlt), times = sapply(rlt, length))
				cs = rep(seq_along(clt), times = sapply(clt, length))
				if(length(rlt) == 1) rs = NULL
				if(length(clt) == 1) cs = NULL

				m = ht_current_full@matrix
				subm = m[ri, ci, drop = FALSE]

				if(show_annotation) {
					top_annotation = ht_current_full@top_annotation
					if(!is.null(top_annotation)) {
						ind_subsettable = which(sapply(top_annotation@anno_list, function(x) x@subsetable))
						if(length(ind_subsettable)) {
							top_annotation = top_annotation[ci, ind_subsettable]
							top_annotation@anno_list = lapply(top_annotation@anno_list, function(x) {
								x@show_legend = FALSE
								x
							})
						} else {
							top_annotation = NULL
						}
					}
					bottom_annotation = ht_current_full@bottom_annotation
					if(!is.null(bottom_annotation)) {
						ind_subsettable = which(sapply(bottom_annotation@anno_list, function(x) x@subsetable))
						if(length(ind_subsettable)) {
							bottom_annotation = bottom_annotation[ci, ind_subsettable]
							bottom_annotation@anno_list = lapply(bottom_annotation@anno_list, function(x) {
								x@show_legend = FALSE
								x
							})
						} else {
							bottom_annotation = NULL
						}
					}
					left_annotation = ht_current_full@left_annotation
					if(!is.null(left_annotation)) {
						ind_subsettable = which(sapply(left_annotation@anno_list, function(x) x@subsetable))
						if(length(ind_subsettable)) {
							left_annotation = left_annotation[ri, ind_subsettable]
							left_annotation@anno_list = lapply(left_annotation@anno_list, function(x) {
								x@show_legend = FALSE
								x
							})
						} else {
							left_annotation = NULL
						}
					}
					right_annotation = ht_current_full@right_annotation
					if(!is.null(right_annotation)) {
						ind_subsettable = which(sapply(right_annotation@anno_list, function(x) x@subsetable))
						if(length(ind_subsettable)) {
							right_annotation = right_annotation[ri, ind_subsettable]
							right_annotation@anno_list = lapply(right_annotation@anno_list, function(x) {
								x@show_legend = FALSE
								x
							})
						} else {
							right_annotation = NULL
						}
					}
				} else {
					top_annotation = NULL
					bottom_annotation = NULL
					left_annotation = NULL
					right_annotation = NULL
				}

				row_labels = ht_current_full@row_names_param$labels
				if(!is.null(row_labels)) {
					row_labels = row_labels[ri]
				}
				column_labels = ht_current_full@column_names_param$labels
				if(!is.null(column_labels)) {
					column_labels = column_labels[ci]
				}

				if(show_cell_fun) {
					cell_fun = ht_current_full@matrix_param$cell_fun
					if(!is.null(cell_fun)) {
						cell_fun2 = cell_fun
						ri_reverse_map = structure(ri, names = seq_along(ri))
						ci_reverse_map = structure(ci, names = seq_along(ci))
						cell_fun = function(j, i, x, y, w, h, fill) {
							cell_fun2(ci_reverse_map[as.character(j)], 
								ri_reverse_map[as.character(i)], 
								x, y, w, h, fill)
						}
					}
					layer_fun = ht_current_full@matrix_param$layer_fun
					if(!is.null(layer_fun)) {
						layer_fun2 = layer_fun
						ri_reverse_map = structure(ri, names = seq_along(ri))
						ci_reverse_map = structure(ci, names = seq_along(ci))
						layer_fun = function(j, i, x, y, w, h, fill) {
							layer_fun2(ci_reverse_map[as.character(j)], 
								ri_reverse_map[as.character(i)], 
								x, y, w, h, fill)
						}
					}
				} else {
					cell_fun = NULL
					layer_fun = NULL
				}

				ignored_anno = c("anno_oncoprint_barplot", "anno_zoom", "anno_empty")
				if(!is.null(top_annotation)) {
					if(length(top_annotation) == 1) {
						if(top_annotation@anno_list[[1]]@fun@fun_name %in% ignored_anno) {
							top_annotation = NULL
						}
					} else {
						ind = which(sapply(top_annotation@anno_list, function(x) !x@fun@fun_name %in% ignored_anno))
						top_annotation = top_annotation[, ind]
					}
				}
				if(!is.null(bottom_annotation)) {
					if(length(bottom_annotation) == 1) {
						if(bottom_annotation@anno_list[[1]]@fun@fun_name %in% ignored_anno) {
							bottom_annotation = NULL
						}
					} else {
						ind = which(sapply(bottom_annotation@anno_list, function(x) !x@fun@fun_name %in% ignored_anno))
						bottom_annotation = bottom_annotation[, ind]
					}
				}
				if(!is.null(left_annotation)) {
					if(length(left_annotation) == 1) {
						if(left_annotation@anno_list[[1]]@fun@fun_name %in% ignored_anno) {
							left_annotation = NULL
						}
					} else {
						ind = which(sapply(left_annotation@anno_list, function(x) !x@fun@fun_name %in% ignored_anno))
						left_annotation = left_annotation[, ind]
					}
				}
				if(!is.null(right_annotation)) {
					if(length(right_annotation) == 1) {
						if(right_annotation@anno_list[[1]]@fun@fun_name %in% ignored_anno) {
							right_annotation = NULL
						}
					} else {
						ind = which(sapply(right_annotation@anno_list, function(x) !x@fun@fun_name %in% ignored_anno))
						right_annotation = right_annotation[, ind]
					}
				}

				if(any(c("", attr(ht_current_full, "translate_from")) %in% c("heatmap", "heatmap.2"))) {
					if(!is.null(right_annotation)) {
						if(show_row_names) {
							show_row_names = FALSE
						} else {
							right_annotation = right_annotation[, "ylab"]
						}
					}
					if(!is.null(bottom_annotation)) {
						if(show_column_names) {
							show_column_names = FALSE
						} else {
							bottom_annotation = bottom_annotation[, "xlab"]
						}
					}
				}

				heatmap_width = unit(1, "npc")
				body_width = NULL
				heatmap_height = unit(1, "npc")
				body_height = NULL
				if(is_abs_unit(ht_current_full@heatmap_param$width)) {
					heatmap_width = ht_current_full@heatmap_param$width
				}
				if(is_abs_unit(ht_current_full@heatmap_param$height)) {
					heatmap_height = ht_current_full@heatmap_param$height
				}
				if(is_abs_unit(ht_current_full@matrix_param$width)) {
					body_width = ht_current_full@matrix_param$width
					heatmap_width = unit(1, "npc")
				}
				if(is_abs_unit(ht_current_full@matrix_param$height)) {
					body_height = ht_current_full@matrix_param$height
					heatmap_height = unit(1, "npc")
				}
				
				ht_current = Heatmap(subm, rect_gp = ht_current_full@matrix_param$gp,
					row_split = rs, column_split = cs,
			    	col = ht_current_full@matrix_color_mapping,
			    	show_heatmap_legend = FALSE,
			    	cluster_rows = FALSE, cluster_columns = FALSE,
					row_title = NULL, column_title = NULL,
					border = ht_current_full@matrix_param$border,
					row_labels = row_labels, column_labels = column_labels,
					show_row_names = show_row_names, row_names_side = ht_current_full@row_names_param$side,
					show_column_names = show_column_names, column_names_side = ht_current_full@column_names_param$side,
					top_annotation = top_annotation,
					bottom_annotation = bottom_annotation,
					left_annotation = left_annotation,
					right_annotation = right_annotation,
					cell_fun = cell_fun, layer_fun = layer_fun,
					heatmap_width = heatmap_width, width = body_width,
					heatmap_height = heatmap_height, height = body_height
				)
				
			} else {
				if(show_annotation) {
					ha = ht_current_full
					ind_subsettable = which(sapply(ha@anno_list, function(x) x@subsetable && !x@fun@fun_name %in% ignored_anno))
					if(length(ind_subsettable)) {
						if(ht_list@direction == "horizontal") {
							ha = ha[ri, ind_subsettable]
						} else {
							ha = ha[ci, ind_subsettable]
						}
						ha@anno_list = lapply(ha@anno_list, function(x) {
							x@show_legend = FALSE
							x
						})
						ht_current = ha
					} else {
						ht_current = NULL
					}
				} else {
					ht_current = NULL
				}
			}

			if(ht_list@direction == "horizontal") {
				ht_select = ht_select + ht_current
					
			} else {
				ht_select = ht_select %v% ht_current
			}
		}
	    draw(ht_select)
	    message(qq("[@{Sys.time()}] make the sub-heatmap (device size: @{width}x@{height} px)."))
	}
}


default_brush_action = function(input, output, session, heatmap_id) {
	output[[qq("@{heatmap_id}_info")]] = renderUI({
		selected = shiny_env[[heatmap_id]]$selected
		if(is.null(selected)) {
			HTML("<p>Selected area should overlap to heatmap bodies.</p>")
		} else {

			selected = selected[!is.na(selected$row_slice), ]

			n_ht = length(unique(selected$heatmap))

			ht_list = shiny_env[[heatmap_id]]$ht_list
			if(ht_list@direction == "horizontal") {
				l1 = !duplicated(selected$row_slice)
				nr = length(unlist(selected$row_index[l1]))

				l2 = !duplicated(paste0(selected$heatmap, selected$column_slice))
				nc = length(unlist(selected$column_index[l2]))
			} else {
				l1 = !duplicated(paste0(selected$heatmap, selected$row_slice))
				nr = length(unlist(selected$row_index[l1]))

				l2 = !duplicated(selected$column_slice)
				nc = length(unlist(selected$column_index[l2]))
			}

			selected_df = as.data.frame(selected)
			shiny_env$history[[ digest(selected_df) ]] = selected_df

			con = textConnection("dump_txt", "w")
			dump("selected_df", file = con)
			close(con)
			dump_txt = dump_txt[-1]
			dump_txt = paste(dump_txt, collapse = "\n")
			HTML(paste(
				  qq("<p>Selected over @{n_ht} heatmap@{ifelse(n_ht > 1, 's', '')} with @{nr} row@{ifelse(nr > 1, 's', '')} and @{nc} column@{ifelse(nc > 1, 's', '')}. Row and column indices can be obtained by copying following code:</p>"),
				  qq("<p><input id='@{heatmap_id}_show_code' type='button' value='show/hide code' /></p>"),
				  qq("<pre id='@{heatmap_id}_code'>"),
				  dump_txt,
				  "</pre>",
				  "<script>",
				  qq("$('#@{heatmap_id}_code').hide();"),
				  qq("$('#@{heatmap_id}_show_code').click(function(){ $('#@{heatmap_id}_code').toggle(); });"),
				  "</script>",
				  
				  sep = "\n"))
		}
	})
}

default_click_action = function(input, output, session, heatmap_id) {
	output[[qq("@{heatmap_id}_info")]] = renderUI({

	    if(is.null(shiny_env[[heatmap_id]]$selected)) {
	    	HTML("<p>No position is selected.</p>")
	    } else {
	    	showNotification(qq("Click on the heatmap."), duration = 2, type = "message")
	    	pos = shiny_env[[heatmap_id]]$selected

			if(is.null(pos)) {
				HTML("<p>You did not select inside the heatmap.</p>")
			} else {
				ht_name = pos[1, "heatmap"]
				slice_name = pos[1, "slice"]

				ht_list = shiny_env[[heatmap_id]]$ht_list
		
				row_index = pos[1, "row_index"][[1]]
			    column_index = pos[1, "column_index"][[1]]
			    m = ht_list@ht_list[[ht_name]]@matrix
			    v = m[row_index, column_index]

			    if(is.null(ht_list@ht_list[[ht_name]]@heatmap_param$oncoprint_env)) {
			    	col = map_to_colors(ht_list@ht_list[[ht_name]]@matrix_color_mapping, v)
			    } else {
			    	col = NA
			    }
			    if(is.na(v)) v = "NA"
			    row_label = rownames(m)[row_index]
			    column_label = colnames(m)[column_index]
			    if(is.null(row_label)) {
			    	row_label = "NULL"
			    } else {
			    	# row_label = paste0("'", row_label, "'")
			    }
			    if(is.null(column_label)) {
			    	column_label = "NULL"
			    } else {
			    	# column_label = paste0("'", column_label, "'")
			    }

			    message(qq("[@{Sys.time()}] click on the heatmap @{slice_name}."))
				
				HTML(paste("<p>Information of the clicked position:</p>",
					  "<pre>",
					  qq("heatmap: @{ht_name}"),
					  qq("heatmap slice: @{slice_name}"),
					  qq("row index: @{row_index}"),
					  qq("row label: @{row_label}"),
					  qq("column index: @{column_index}"),
					  qq("column_label: @{column_label}"),
					  ifelse(is.na(col), qq("value: @{v}"), qq("value: @{v} <span style='background-color:@{col};width=10px;'>    </span>")),
					  "</pre>",
					  sep = "\n"))
			}
		}
	})
}
