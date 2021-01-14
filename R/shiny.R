
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
# == value
# A UI that can be used in shiny.
InteractiveComplexHeatmapOutput = function(heatmap_id = NULL, 
	title1 = "Original heatmap", title2 = "Selected sub-heatmap",
	width1 = 400, height1 = 350, width2 = 370, height2 = 350, nrow = 1,
	action = c("click", "hover", "dblclick"), brush_opt = list(), 
	output_div = TRUE, css = "") {

	if(is.null(heatmap_id)) {
		heatmap_id = paste0("ht_", digest(Sys.time()))
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

		tags$head(
			tags$link(rel = "stylesheet", type = "text/css", href = "shared/fontawesome/css/all.min.css"),
			tags$link(rel = "stylesheet", type = "text/css", href = "shared/fontawesome/css/v4-shims.min.css")
		),

		tags$script(HTML(paste(readLines(system.file("app", "jquery-ui.min.js", package = "InteractiveComplexHeatmap"), warn = FALSE), collapse = "\n"))),

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
		tags$style(paste(readLines(system.file("app", "jquery-ui.min.css", package = "InteractiveComplexHeatmap"), warn = FALSE), collapse = "\n")),
		tags$style(qq("
#@{heatmap_id}_heatmap_wrap_outer, #@{heatmap_id}_sub_heatmap_wrap_outer {
	@{ifelse(nrow == 1, 'float:left;', '')}
	margin-bottom: 10px;
}
#@{heatmap_id}_heatmap_wrap_outer {
	margin-right: 10px;
}
#@{heatmap_id}_heatmap_wrap {
	width: @{width1+4}px;
	height: @{height1+4}px;
	position:relative;
	border:1px solid grey;
	text-align:center;
}
#@{heatmap_id}_sub_heatmap_wrap {
	width: @{width2+4}px;
	height: @{height2+4}px;
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
}
.ui-icon, .ui-widget-content .ui-icon {
    background-image: url('data:image/png;base64,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');
}
.ui-state-default, .ui-widget-content .ui-state-default, .ui-widget-header .ui-state-default, .ui-button, html .ui-button.ui-state-disabled:hover, html .ui-button.ui-state-disabled:active {
	border: 0px;
}
.ui-tabs .ui-tabs-nav {
	padding: 0px;
}
.ui-widget-header {
	border: 0px;
	background: white;
}
.ui-widget.ui-widget-content {
	border: 1px;
}
.ui-tabs .ui-tabs-panel {
    padding: 1em 1.4em;
	border: 1px solid #ddd;
}
.ui-state-active, 
.ui-widget-content .ui-state-active, 
.ui-widget-header .ui-state-active, 
a.ui-button:active, 
.ui-button:active, 
.ui-button.ui-state-active:hover {
    border: 0px;
    background: #CCCCCC;
    font-weight: normal;
    color: black;
}
.ui-state-focus {
	border: 0px;
}
.ui-state-focus a {
	outline: #CCCCCC solid 0px;
}
.ui-tabs .ui-tabs-nav li {
	float: right;
	margin: 1px 0em 0 0.2em;
}
@{css}
")),
	div(
		h5(title1),
		div(
			plotOutput(qq("@{heatmap_id}_heatmap"), height = height1, width = width1,
				        brush = do.call(brushOpts, c(list(id = qq("@{heatmap_id}_heatmap_brush")), brush_opt)),
				        click = click, dblclick = dblclick, hover = hover
			),
			tags$script(HTML(qq("
				$('#@{heatmap_id}_heatmap').html('<p style=\"position:relative;top:50%;\">Making heatmap, please wait...</p>');
			"))),
			id = qq("@{heatmap_id}_heatmap_wrap")
		),
		htmlOutput(qq("@{heatmap_id}_heatmap_control")),
		HTML(qq("<div style='display: none;'><a id='@{heatmap_id}_heatmap_download_button'></a></div>")),
		id = qq("@{heatmap_id}_heatmap_wrap_outer")
	),
	div(
		h5(title2),
		div(
			plotOutput(qq("@{heatmap_id}_sub_heatmap"), height = height2, width = width2),
			id = qq("@{heatmap_id}_sub_heatmap_wrap")
		),
		htmlOutput(qq("@{heatmap_id}_sub_heatmap_control")),
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

	shiny_env[[heatmap_id]] = list(count = 0)

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
			shiny_env[[heatmap_id]]$ht_pos = htPositionsOnDevice(shiny_env[[heatmap_id]]$ht_list, include_annotation = TRUE, calibrate = FALSE)
			shiny_env[[heatmap_id]]$count = shiny_env[[heatmap_id]]$count + 1
			shiny_env[[heatmap_id]]$selected = NULL

			if(default_click_action || default_brush_action) {
				output[[qq("@{heatmap_id}_info")]] = renderUI({
					HTML("<h5>Output</h5>\n<p>No position is selected.</p>")
				})
			}
			message(qq("[@{Sys.time()}] make the original heatmap and calculate positions (device size: @{width}x@{height} px)."))
		})

		output[[qq("@{heatmap_id}_heatmap_control")]] = renderUI({
			if(!is.null(shiny_env[[heatmap_id]]$ht_list)) {
				heatmap_control_ui(heatmap_id, shiny_env)
			}
		})
		
		updateNumericInput(session, qq("@{heatmap_id}_heatmap_input_width"), value = session$clientData[[qq("output_@{heatmap_id}_heatmap_width")]])
		updateNumericInput(session, qq("@{heatmap_id}_heatmap_input_height"), value = session$clientData[[qq("output_@{heatmap_id}_heatmap_heigh")]])
		updateNumericInput(session, qq("@{heatmap_id}_heatmap_download_image_width"), value = session$clientData[[qq("output_@{heatmap_id}_heatmap_width")]])
		updateNumericInput(session, qq("@{heatmap_id}_heatmap_download_image_height"), value = session$clientData[[qq("output_@{heatmap_id}_heatmap_heigh")]])
	})

	observeEvent(input[[qq("@{heatmap_id}_heatmap_download_trigger")]], {
		output[[qq("@{heatmap_id}_heatmap_download_button")]] = downloadHandler(

			filename = function() {

				format = as.numeric(input[[qq("@{heatmap_id}_heatmap_download_format")]])
				fm = c("png", "pdf", "svg")[format]
				qq("@{heatmap_id}_heatmap.@{fm}")
			},
			content = function(file) {
				
				format = as.numeric(input[[qq("@{heatmap_id}_heatmap_download_format")]])
				fm = c("png", "pdf", "svg")[format]
				dev = list(png, pdf, svglite::svglite)[[format]]

				showNotification(qq("Download heatmap in @{fm}."), duration = 2, type = "message")
				message(qq("[@{Sys.time()}] Download heatmap in @{fm}."))

				temp = tempfile()
				width = input[[qq("@{heatmap_id}_heatmap_download_image_width")]]
				height = input[[qq("@{heatmap_id}_heatmap_download_image_height")]]
				
				if(fm == "png") {
					dev(temp, width = width*2, height = height*2, res = 72*2)
				} else {
					dev(temp, width = width, height = height)
				}
				if(!is.null(shiny_env[[heatmap_id]]$ht_list)) {
			    	draw(shiny_env[[heatmap_id]]$ht_list)
			    } else {
			    	grid.newpage()
			    	grid.text("No heatmap is available.")
			    }
			    dev.off()

				file.copy(temp, file)
			}
		)
	})
	

	observeEvent(input[[qq("@{heatmap_id}_heatmap_input_size_button")]], {

		if(!is.null(shiny_env[[heatmap_id]]$ht_list)) {

			output[[qq("@{heatmap_id}_heatmap")]] = renderPlot({
				width = input[[qq("@{heatmap_id}_heatmap_input_width")]]
		    	height = input[[qq("@{heatmap_id}_heatmap_input_height")]]
		    	
		    	showNotification("Resizing the original heatmap.", duration = 2, type = "message")

		    })
		}
	})

	# default
	output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
		grid.newpage()
		grid.text("No area on the heatmap is selected.", 0.5, 0.5, gp = gpar(fontsize = 14))

		message(qq("[@{Sys.time()}] no area on the heatmap is selected, Do not make the sub-heatmap."))
	})

	if(default_click_action || default_brush_action) {
		output[[qq("@{heatmap_id}_info")]] = renderUI({
			HTML("<h5>Output</h5>\n<p>No position is selected.</p>")
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
				grid.text("No area on the heatmap is selected.", 0.5, 0.5, gp = gpar(fontsize = 14))
    		} else {
    			make_sub_heatmap(input, output, session, heatmap_id)
			}
		})

		output[[qq("@{heatmap_id}_sub_heatmap_control")]] = renderUI({
			if(!is.null(shiny_env[[heatmap_id]]$selected)) {
				sub_heatmap_control_ui(heatmap_id)
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
				grid.text("Query keyword is empty.", 0.5, 0.5, gp = gpar(fontsize = 14, col = "red"))
			})

			if(default_brush_action) {
				default_brush_action(input, output, session, heatmap_id, "Query keyword is empty.")
			}

			if(!is.null(brush_action)) {
				brush_action(shiny_env[[heatmap_id]]$selected, output)
			}

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
				grid.text("No heatmap is selected for searching.", 0.5, 0.5, gp = gpar(fontsize = 14, col = "red"))
			})

			if(default_brush_action) {
				default_brush_action(input, output, session, heatmap_id, "No heatmap is selected for searching.")
			}

			if(!is.null(brush_action)) {
				brush_action(shiny_env[[heatmap_id]]$selected, output)
			}
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
			selected = selectByLabels(ht_list, row_keywords = keywords, keyword_is_regexpr = is_regexpr, include_annotation = TRUE, heatmap = sht, all = length(extend))
		} else if(where == 2) {
			selected = selectByLabels(ht_list, column_keywords = keywords, keyword_is_regexpr = is_regexpr, include_annotation = TRUE, heatmap = sht, all = length(extend))
		} else {
			selected = selectByLabels(ht_list, row_keywords = keywords, column_keywords = keywords, keyword_is_regexpr = is_regexpr, include_annotation = TRUE, heatmap = sht, all = length(extend))
		}
		shiny_env[[heatmap_id]]$selected = selected

		output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
			
    		if(is.null(shiny_env[[heatmap_id]]$selected)) {
    			grid.newpage()
				grid.text(paste(strwrap(qq("Found nothing from heatmaps with keywords '@{keywords2}'."), width = 60), collapse = "\n"), 0.5, 0.5, gp = gpar(fontsize = 14, col = "red"))

				if(default_brush_action) {
					default_brush_action(input, output, session, heatmap_id, qq("Found nothing from heatmaps with keywords '@{keywords2}'."))
				}

				if(!is.null(brush_action)) {
					brush_action(shiny_env[[heatmap_id]]$selected, output)
				}
				return(invisible(NULL))
    		} else {
    			make_sub_heatmap(input, output, session, heatmap_id)
			}
		})

		output[[qq("@{heatmap_id}_sub_heatmap_control")]] = renderUI({
			if(!is.null(shiny_env[[heatmap_id]]$selected)) {
				sub_heatmap_control_ui(heatmap_id)
			}
		})

		if(default_brush_action) {
			default_brush_action(input, output, session, heatmap_id)
		}

		if(!is.null(brush_action)) {
			brush_action(shiny_env[[heatmap_id]]$selected, output)
		}
	
	})

	observeEvent(input[[qq("@{heatmap_id}_sub_heatmap_download_trigger")]], {
		output[[qq("@{heatmap_id}_sub_heatmap_download_button")]] = downloadHandler(

			filename = function() {

				format = as.numeric(input[[qq("@{heatmap_id}_sub_heatmap_download_format")]])
				fm = c("png", "pdf", "svg")[format]
				qq("@{heatmap_id}_sub_heatmap.@{fm}")
			},
			content = function(file) {
				
				format = as.numeric(input[[qq("@{heatmap_id}_heatmap_download_format")]])
				fm = c("png", "pdf", "svg")[format]
				dev = list(png, pdf, svglite::svglite)[[format]]

				showNotification(qq("Download sub-heatmap in @{fm}."), duration = 2, type = "message")
				message(qq("[@{Sys.time()}] Download sub-heatmap in @{fm}."))

				temp = tempfile()
				width = input[[qq("@{heatmap_id}_sub_heatmap_download_image_width")]]
				height = input[[qq("@{heatmap_id}_sub_heatmap_download_image_height")]]
				
				if(fm == "png") {
					dev(temp, width = width*2, height = height*2, res = 72*2)
				} else {
					dev(temp, width = width, height = height)
				}
				if(is.null(shiny_env[[heatmap_id]]$selected)) {
	    			grid.newpage()
					grid.text("No heatmap is available.")
	    		} else {
	    			make_sub_heatmap(input, output, session, heatmap_id)
				}
			    dev.off()

				file.copy(temp, file)
			}
		)
	})

	observeEvent(input[[qq("@{heatmap_id}_sub_heatmap_input_size_button")]], {
		
		output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
			if(is.null(shiny_env[[heatmap_id]]$selected)) {
    			grid.newpage()
				grid.text("No area on the heatmap is selected.", 0.5, 0.5, gp = gpar(fontsize = 14))
    		} else {
    			make_sub_heatmap(input, output, session, heatmap_id, update_size = FALSE)
			}
		})
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

		output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
			grid.newpage()
			grid.text("No area on the heatmap is selected.", 0.5, 0.5, gp = gpar(fontsize = 14))
		})

		output[[qq("@{heatmap_id}_sub_heatmap_control")]] = renderUI({
			NULL
		})
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

		updateRadioButtons(session, qq("@{heatmap_id}_search_where"), label = "Which dimension to search?", choices = where_choices, selected = where_choices[[1]], inline = TRUE)

	})

	observeEvent(input[[qq("@{heatmap_id}_open_table")]], {
		if(is.null(shiny_env[[heatmap_id]]$selected)) {
			showModal(modalDialog(
				title = "The selected tables",
				p("Rows or columns are not selected."),
				tags$script(HTML("$('.modal-content').draggable();")),
				easyClose = TRUE,
				footer = modalButton("Close")
			))
		} else {
			showModal(modalDialog(
				title = "The selected tables",
				htmlOutput(qq("@{heatmap_id}_selected_table")),
				div(
					numericInput(qq("@{heatmap_id}_digits"), "Digits of numeric values:", value = 2, min = 0),
					style = "margin-top:5px"
				),
				tags$script(HTML("
					$('.modal-content').draggable();
					$('.modal-content label').css('display', 'table-cell').css('text-align', 'center').css('vertical-align', 'middle').css('padding-right', '10px');
					$('.modal-content .form-group').css('display', 'table-row');
					$('.modal-content input').css('width', '100px');
				")),
				easyClose = TRUE,
				footer = div(downloadButton(qq("@{heatmap_id}_download_table"), "Download"), modalButton("Close")),
				size = "l"
			))

			output[[qq("@{heatmap_id}_selected_table")]] = renderUI({
				HTML(format_html_table(heatmap_id))
			})

		}
	})

	format_html_table = function(heatmap_id, digits = 2) {
		tb = get_sub_matrix(heatmap_id, digits = round(digits))
		is_cn = attr(tb, "is_cn")
		is_rn = attr(tb, "is_rn")
		hline = attr(tb, "hline")
		vline = attr(tb, "vline")

		kb = kbl(tb, format = "html")
		for(i in which(is_rn)) {
			kb = column_spec(kb, i, bold = TRUE, background = "#EFEFEF")
		}
		for(i in which(is_cn)) {
			kb = row_spec(kb, i, bold = TRUE, background = "#EFEFEF")
		}
		kb = column_spec(kb, 1, border_left = TRUE)
		for(i in which(vline)) {
			kb = column_spec(kb, i, border_right = TRUE)
		}
		for(i in which(hline)) {
			kb = row_spec(kb, i, extra_css = "border-bottom: 1px solid")
		}
		kb = row_spec(kb, 1, extra_css = "border-top: 1px solid")
		
		kb = scroll_box(
			kable_styling(kb, full_width = FALSE, position = "left"), 
			width = "100%",
			box_css = "border: 1px solid #ddd; padding: 5px; max-height:500px;"
		)
		kb
	}

	observeEvent(input[[qq("@{heatmap_id}_digits")]], {

		output[[qq("@{heatmap_id}_selected_table")]] = renderUI({
			HTML(format_html_table(heatmap_id, input[[qq("@{heatmap_id}_digits")]]))
		})
	})

	output[[qq("@{heatmap_id}_download_table")]] = downloadHandler(
		filename = function() {
			qq("@{heatmap_id}_download_table.csv")
		},
		content = function(file) {
			tb = get_sub_matrix(heatmap_id)
			write.table(tb, file, row.names = FALSE, col.names = FALSE, sep = ",", quote = TRUE)
		}
	)
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

make_sub_heatmap = function(input, output, session, heatmap_id, update_size = TRUE) {
	showNotification("Making the selected sub-heatmap.", duration = 2, type = "message")

	width = session$clientData[[qq("output_@{heatmap_id}_sub_heatmap_width")]]
    height = session$clientData[[qq("output_@{heatmap_id}_sub_heatmap_height")]]

	show_row_names = input[[qq("@{heatmap_id}_show_row_names_checkbox")]]
	show_column_names = input[[qq("@{heatmap_id}_show_column_names_checkbox")]]
	show_annotation = input[[qq("@{heatmap_id}_show_annotation_checkbox")]]
	show_cell_fun = input[[qq("@{heatmap_id}_show_cell_fun_checkbox")]]

	if(is.null(show_row_names)) show_row_names = TRUE
	if(is.null(show_column_names)) show_column_names = TRUE
	if(is.null(show_annotation)) show_annotation = TRUE
	if(is.null(show_cell_fun)) show_cell_fun = TRUE

	selected = shiny_env[[heatmap_id]]$selected
    if(is.null(selected)) {
    	grid.newpage()
		grid.text("Selected area should overlap to heatmap bodies", 0.5, 0.5, gp = gpar(fontsize = 14))
    } else {

    	all_ht_name = unique(selected$heatmap)

    	ht_list = shiny_env[[heatmap_id]]$ht_list

    	ignored_anno = c("anno_oncoprint_barplot", "anno_zoom", "anno_empty")

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
					# if(ComplexHeatmap:::is_abs_unit(body_width)) {
					# 	body_width = body_width * (length(ci)/ncol(m))
					# } 
					heatmap_width = unit(1, "npc")
				}
				if(is_abs_unit(ht_current_full@matrix_param$height)) {
					body_height = ht_current_full@matrix_param$height
					# if(ComplexHeatmap:::is_abs_unit(body_height)) {
					# 	body_height = body_height * (length(ri)/nrow(m))
					# }
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
							selected_ht = selected[selected$heatmap == selected$heatmap[!is.na(selected$slice)][1], ]
			    			l1 = !duplicated(selected_ht$row_slice)
			    			rlt = selected_ht$row_index[l1]
			    			ri = unlist(rlt)
							
							ha = ha[ri, ind_subsettable]
						} else {
							selected_ht = selected[selected$heatmap == selected$heatmap[!is.na(selected$slice)][1], ]
			    			l2 = !duplicated(selected_ht$column_slice)
			    			clt = selected_ht$column_index[l2]

			    			ci = unlist(clt)
							
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
	    ht_select = draw(ht_select, save_last = FALSE)
	    message(qq("[@{Sys.time()}] make the sub-heatmap (device size: @{width}x@{height} px)."))
	}

	if(update_size) {
		updateNumericInput(session, qq("@{heatmap_id}_sub_heatmap_input_width"), value = session$clientData[[qq("output_@{heatmap_id}_sub_heatmap_width")]])
		updateNumericInput(session, qq("@{heatmap_id}_sub_heatmap_input_height"), value = session$clientData[[qq("output_@{heatmap_id}_sub_heatmap_height")]])
	}
}

# if annotation is included, top/bottom annotation are all put at the bottom of the matrix
get_sub_matrix = function(heatmap_id, digits = 2, include_annotation = TRUE) {
	message(qq("[@{Sys.time()}] fetch selected tables."))

	dev.null()
	on.exit(dev.off2())

	selected = shiny_env[[heatmap_id]]$selected
	all_ht_name = unique(selected$heatmap)

	ht_list = shiny_env[[heatmap_id]]$ht_list

	data_anno = c("anno_points", "anno_lines", "anno_barplot", "anno_text", "anno_simple")

	mat_list = list()
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

			if(is.numeric(subm)) subm = round(subm, digits)

			if(include_annotation) {
				top_annotation_data = NULL
				top_annotation = ht_current_full@top_annotation
				if(!is.null(top_annotation)) {
					ind_data = which(sapply(top_annotation@anno_list, function(x) x@fun@fun_name %in% data_anno))
					if(length(ind_data)) {
						top_annotation = top_annotation[ci, ind_data]
						top_annotation_data = collect_data_frame_from_anno(top_annotation, digits, direction = "vertical")
					}
				}
				bottom_annotation_data = NULL
				bottom_annotation = ht_current_full@bottom_annotation
				if(!is.null(bottom_annotation)) {
					ind_data = which(sapply(bottom_annotation@anno_list, function(x) x@fun@fun_name %in% data_anno))
					if(length(ind_data)) {
						bottom_annotation = bottom_annotation[ci, ind_data]
						bottom_annotation_data = collect_data_frame_from_anno(bottom_annotation, digits, direction = "vertical")
					}
				}
				left_annotation_data = NULL
				left_annotation = ht_current_full@left_annotation
				if(!is.null(left_annotation)) {
					ind_data = which(sapply(left_annotation@anno_list, function(x) x@fun@fun_name %in% data_anno))
					if(length(ind_data)) {
						left_annotation = left_annotation[ri, ind_data]
						left_annotation_data = collect_data_frame_from_anno(left_annotation, digits, direction = "horizontal")
					}
				}
				right_annotation_data = NULL
				right_annotation = ht_current_full@right_annotation
				if(!is.null(right_annotation)) {
					ind_data = which(sapply(right_annotation@anno_list, function(x) x@fun@fun_name %in% data_anno))
					if(length(ind_data)) {
						right_annotation = right_annotation[ri, ind_data]
						right_annotation_data = collect_data_frame_from_anno(right_annotation, digits, direction = "horizontal")
					}
				}

				column_annotation_data = rbind(top_annotation_data, bottom_annotation_data)
				row_annotation_data = cbind(left_annotation_data, right_annotation_data)
				attr(subm, "column_annotation_data") = column_annotation_data
				attr(subm, "row_annotation_data") = row_annotation_data
			}

			mat_list[[ht_name]] = subm
		} else {
			if(include_annotation) {
				ha = ht_current_full
				ind_data = which(sapply(ha@anno_list, function(x) x@fun@fun_name %in% data_anno))
				if(length(ind_data)) {
					if(ht_list@direction == "horizontal") {
						if(!exists("ri")) {
							selected_ht = selected[selected$heatmap == selected$heatmap[!is.na(selected$slice)][1], ]
			    			l1 = !duplicated(selected_ht$row_slice)
			    			rlt = selected_ht$row_index[l1]
			    			
			    			ri = unlist(rlt)
						}
						ha = ha[ri, ind_data]
						mat_list[[ht_name]] = collect_data_frame_from_anno(ha, digits, direction = "horizontal")
					} else {
						if(!exists("ci")) {
							selected_ht = selected[selected$heatmap == selected$heatmap[!is.na(selected$slice)][1], ]
			    			l2 = !duplicated(selected_ht$column_slice)
			    			clt = selected_ht$column_index[l2]

			    			ci = unlist(clt)
						}
						ha = ha[ci, ind_data]
						mat_list[[ht_name]] = collect_data_frame_from_anno(ha, digits, direction = "vertical")
					}

					attr(mat_list[[ht_name]], "anno") = TRUE
				} 
			}
		}
	}

	mat_list2 = lapply(mat_list, function(m) {
		dim = dim(m)
		from_anno = attr(m, "anno")
		if(is.null(from_anno)) {
			rn = rownames(m)
			cn = colnames(m)
			row_annotation_data = attr(m, "row_annotation_data")
			column_annotation_data = attr(m, "column_annotation_data")
			
			if(is.null(rn)) rn = rep("", nrow(m))
			if(is.null(cn)) cn = rep("", ncol(m))

			hline = c(rep(FALSE, nrow(m)-1), TRUE)

			m = rbind(cn, m)
			hline = c(FALSE, hline)
			rn = c("", rn)
			m = cbind(rn, m)
			vline = c(rep(FALSE, ncol(m)-1), TRUE)

			if(!is.null(row_annotation_data)) {
				m = cbind(m, rbind(colnames(row_annotation_data), row_annotation_data))
				vline = c(vline, c(rep(FALSE, ncol(row_annotation_data) - 1), TRUE))
			}

			if(!is.null(column_annotation_data)) {
				m = rbind(m, cbind(rownames(column_annotation_data), cbind(column_annotation_data, matrix("", nrow = nrow(column_annotation_data), ncol = ncol(m) - ncol(column_annotation_data) - 1))))
				hline = c(hline, c(rep(FALSE, nrow(column_annotation_data) - 1), TRUE))
			}

			dimnames(m) = NULL
		} else {
			if(ht_list@direction == "horizontal") {
				cn = colnames(m)
				m = rbind(cn, m)
			} else {
				rn = rownames(m)
				m = cbind(rn, m)
			}
			vline = c(rep(FALSE, ncol(m) - 1), TRUE)
			hline = c(rep(FALSE, nrow(m) - 1), TRUE)
			dimnames(m) = NULL
		}

		attr(m, "original_dim") = dim
		attr(m, "hline") = hline
		attr(m, "vline") = vline
		attr(m, "anno") = from_anno
		m
	})

	if(ht_list@direction == "horizontal") {
		nr = max(sapply(mat_list2, nrow))

		tb = do.call(cbind, lapply(mat_list2, function(m) {
			if(nrow(m) < nr) {
				m = rbind(m, matrix("", nrow = nr - nrow(m), ncol = ncol(m)))
			}
			m
		}))
		is_cn = c(TRUE, rep(FALSE, nr - 1))
		is_rn = unlist(lapply(mat_list2, function(m) {
			if(is.null(attr(m, "anno"))) {
				c(TRUE, rep(FALSE, ncol(m) - 1))
			} else {
				rep(FALSE, ncol(m))
			}
		}))

		hline = lapply(mat_list2, function(x) attr(x, "hline"))
		vline = lapply(mat_list2, function(x) attr(x, "vline"))
		hline = hline[[which.max(sapply(hline, length))[1]]]
		vline = unlist(vline)

		if(all(tb[1, ] == "")) {
			is_cn = is_cn[-1]
			tb = tb[-1, , drop = FALSE]
			hline = hline[-1]
		}
		l = apply(tb, 2, function(x) all(x == ""))
		tb = tb[, !l, drop = FALSE]
		is_rn = is_rn[!l]
		vline = vline[!l]
	} else {
		nc = max(sapply(mat_list2, ncol))

		tb = do.call(rbind, lapply(mat_list2, function(m) {
			if(ncol(m) < nc) {
				m = cbind(m, matrix("", ncol = nc - ncol(m), nrow = nrow(m)))
			}
			m
		}))
		is_rn = c(TRUE, rep(FALSE, nc - 1))
		is_cn = unlist(lapply(mat_list2, function(m) {
			if(is.null(attr(m, "anno"))) {
				c(TRUE, rep(FALSE, nrow(m) - 1))
			} else {
				rep(FALSE, nrow(m))
			}
		}))

		hline = lapply(mat_list2, function(x) attr(x, "hline"))
		vline = lapply(mat_list2, function(x) attr(x, "vline"))
		vline = vline[[which.max(sapply(vline, length))[1]]]
		hline = unlist(hline)

		if(all(tb[, 1] == "")) {
			is_rn = is_rn[-1]
			tb = tb[, -1, drop = FALSE]
			vline = vline[-1]
		}
		l = apply(tb, 1, function(x) all(x == ""))
		tb = tb[!l, , drop = FALSE]
		is_cn = is_cn[!l]
		hline = hline[!l]
	}

	attr(tb, "is_cn") = is_cn
	attr(tb, "is_rn") = is_rn
	attr(tb, "hline") = hline
	attr(tb, "vline") = vline
	return(tb)
}

collect_data_frame_from_anno = function(ha, digits, direction) {
	lt = lapply(ha@anno_list, function(x) {
		nm = x@name
		v = x@fun@var_env$value
		if(is.matrix(v) || is.data.frame(v)) {
			v = as.matrix(v)
			if(is.null(colnames(v))) {
				nm = paste0(nm, seq_len(ncol(v)))
			} else {
				nm = colnames(v)
			}
		}
		v = as.matrix(v)
		if(is.numeric(v)) v = round(v, digits)
		colnames(v) = nm
		rownames(v) = NULL
		v
	})

	df = do.call(cbind, lt)

	if(direction == "vertical") {
		df = t(as.matrix(df))
	}
	as.matrix(df)
}

default_brush_action = function(input, output, session, heatmap_id,
	default_text = "Selected area should overlap to heatmap bodies.") {
	output[[qq("@{heatmap_id}_info")]] = renderUI({
		selected = shiny_env[[heatmap_id]]$selected
		if(is.null(selected)) {
			HTML(qq("<h5>Output</h5>\n<p>@{default_text}</p>"))
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
				  qq("<h5>Output</h5>\n<p>Selected over @{n_ht} heatmap@{ifelse(n_ht > 1, 's', '')} with @{nr} row@{ifelse(nr > 1, 's', '')} and @{nc} column@{ifelse(nc > 1, 's', '')}. Row and column indices can be obtained by copying following code:</p>"),
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
	    	HTML("<h5>Output</h5>\n<p>No cell is selected.</p>")
	    } else {
	    	showNotification(qq("Click on the heatmap."), duration = 2, type = "message")
	    	pos = shiny_env[[heatmap_id]]$selected

			if(is.null(pos)) {
				HTML("<h5>Output</h5>\n<p>You did not click inside the heatmap.</p>")
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
				
				HTML(paste("<h5>Output</h5>\n<p>Information of the clicked cell:</p>",
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

heatmap_control_ui = function(heatmap_id, shiny_env) {
	width1 = 400
	height1 = 350
	message(qq("[@{Sys.time()}] build heatmap control ui."))

	div(
		div(id = qq('@{heatmap_id}_tabs'),
			HTML(qq("<ul>
				<li><a href='#@{heatmap_id}_tabs-resize' title='Resize image'><i class='fa fa-expand-arrows-alt'></i></a></li>
				<li><a href='#@{heatmap_id}_tabs-save-image' title='Save image'><i class='fa fa-images'></i></a></li>
				<li><a href='#@{heatmap_id}_tabs-brush' title='Configure brush'><i class='fa fa-brush'></i></a></li>
				<li><a href='#@{heatmap_id}_tabs-search' title='Search in heatmaps'><i class='fa fa-search'></i></a></li>
			</ul>")),
			div(id = qq('@{heatmap_id}_tabs-search'), 
				heatmap_search_ui(heatmap_id, shiny_env)
			),
			div(id = qq('@{heatmap_id}_tabs-save-image'),
				radioButtons(qq("@{heatmap_id}_heatmap_download_format"), label = "Which format?", choices = list("png" = 1, "pdf" = 2, "svg" = 3), selected = 1, inline = TRUE),
				numericInput(qq("@{heatmap_id}_heatmap_download_image_width"), label = "Image width (in px)", value = width1),
				numericInput(qq("@{heatmap_id}_heatmap_download_image_height"), label = "Image height (in px)", value = height1),
				downloadButton(qq("@{heatmap_id}_heatmap_download_button"), "Save image")
			),
			div(id = qq('@{heatmap_id}_tabs-resize'),
				numericInput(qq("@{heatmap_id}_heatmap_input_width"), "Box width", width1),
				numericInput(qq("@{heatmap_id}_heatmap_input_height"), "Box height", height1),
				actionButton(qq("@{heatmap_id}_heatmap_input_size_button"), "Change image size"),
				tags$script(HTML(qq("
			$('#@{heatmap_id}_heatmap_input_size_button').click(function(){
				var width = $('#@{heatmap_id}_heatmap_input_width').val();
				width = parseInt(width);
				var height = $('#@{heatmap_id}_heatmap_input_height').val();
				height = parseInt(height);
				$('#@{heatmap_id}_heatmap_wrap').width(width+4);
				$('#@{heatmap_id}_heatmap').width(width);
				$('#@{heatmap_id}_heatmap img').width(width);
				$('#@{heatmap_id}_heatmap_wrap').height(height+4);
				$('#@{heatmap_id}_heatmap').height(height);
				$('#@{heatmap_id}_heatmap img').height(height);
				$('#@{heatmap_id}_heatmap_download_image_width').val(width);
				$('#@{heatmap_id}_heatmap_download_image_height').val(height);
			});
			$('#@{heatmap_id}_heatmap_download_format').change(function() {
				var width = $('#@{heatmap_id}_heatmap_input_width').val();
				width = parseInt(width);
				var height = $('#@{heatmap_id}_heatmap_input_height').val();
				height = parseInt(height);
				if(parseInt($(this).find('input').filter(':checked').val()) == 2) {
					width_in_inch = Math.round(width*10/100*4/3)/10
					height_in_inch = Math.round(height*10/100*4/3)/10
					$('#@{heatmap_id}_heatmap_download_image_width').val(width_in_inch);
					$('#@{heatmap_id}_heatmap_download_image_height').val(height_in_inch);
					$('#@{heatmap_id}_heatmap_download_image_width').prev().text('Image width (in inch)');
					$('#@{heatmap_id}_heatmap_download_image_height').prev().text('Image height (in inch)');
					Shiny.setInputValue('@{heatmap_id}_heatmap_download_image_width', width_in_inch);
					Shiny.setInputValue('@{heatmap_id}_heatmap_download_image_height', height_in_inch);
				} else {
					$('#@{heatmap_id}_heatmap_download_image_width').val(width);
					$('#@{heatmap_id}_heatmap_download_image_height').val(height);
					$('#@{heatmap_id}_heatmap_download_image_width').prev().text('Image width (in px)');
					$('#@{heatmap_id}_heatmap_download_image_height').prev().text('Image height (in px)');
				}
			});
			Shiny.setInputValue('@{heatmap_id}_heatmap_download_trigger', Math.random());
				")))
			),
			div(id = qq('@{heatmap_id}_tabs-brush'),
				tags$style(HTML(paste(
					readLines(system.file("app", "classic.min.css", package = "InteractiveComplexHeatmap"), warn = FALSE),
					readLines(system.file("app", "monolith.min.css", package = "InteractiveComplexHeatmap"), warn = FALSE),
					readLines(system.file("app", "nano.min.css", package = "InteractiveComplexHeatmap"), warn = FALSE),
					collapse = "\n", sep = "\n"))
				),
				tags$script(HTML(paste(
					readLines(system.file("app", "pickr.min.js", package = "InteractiveComplexHeatmap"), warn = FALSE),
					readLines(system.file("app", "pickr.es5.min.js", package = "InteractiveComplexHeatmap"), warn = FALSE),
					collapse = "\n", sep = "\n"))
				),
				div(
					HTML(qq('
					<div class="form-group shiny-input-container" style="float:left; width:120px;">
					<label>Brush border</label>
					<div id="@{heatmap_id}_color_pickers_border"></div>
					</div>
					<div class="form-group shiny-input-container" style="float:left; width:120px;">
					<label>Brush fill</label>
					<div id="@{heatmap_id}_color_pickers_fill"></div>
					</div>
					<div style="clear:both;"></div>')),
					selectizeInput(qq("@{heatmap_id}_color_pickers_border_width"), label = "Border width", 
						choices = list("1px" = 1, "2px" = 2, "3px" = 3), selected = 1,
						options = list(
							render = I("{
      option: function(item, escape) {
      	return '<div><hr style=\"border-top:' + item.value + 'px solid black;\"></div>'
      }
      }"))),
					sliderInput(qq("@{heatmap_id}_color_pickers_opacity"), label = "Opacity", min = 0, max = 1, value = 0.25)
				),
				tags$script(HTML(qq("     
			var @{heatmap_id}_pickr1 = Pickr.create({
			    el: '#@{heatmap_id}_color_pickers_border',
			    default: '#003366',
			    theme: 'nano',
			    comparison: false,
			    components: {preview: true, hue: true}
			});	
			@{heatmap_id}_pickr1.on('change', (color, source, instance) => {
				$('#@{heatmap_id}_heatmap_brush').css('border-color', color.toHEXA().toString());
				$('#@{heatmap_id}_heatmap').mousedown(function() {
					if($('#@{heatmap_id}_heatmap_brush').length > 0) {
						$('#@{heatmap_id}_heatmap_brush').css('border-color', color.toHEXA().toString());
					}
				});
			});
			var @{heatmap_id}_pickr2 = Pickr.create({
			    el: '#@{heatmap_id}_color_pickers_fill',
			    default: '#99ccff',
			    theme: 'nano',
			    comparison: false,
			    components: {preview: true, hue: true}
			});	
			@{heatmap_id}_pickr2.on('change', (color, source, instance) => {
				$('#@{heatmap_id}_heatmap_brush').css('background-color', color.toHEXA().toString());
				$('#@{heatmap_id}_heatmap').mousedown(function() {
					if($('#@{heatmap_id}_heatmap_brush').length > 0) {
						$('#@{heatmap_id}_heatmap_brush').css('background-color', color.toHEXA().toString());
					}
				});
			});
			$('#@{heatmap_id}_color_pickers_border_width').change(function() {
				var val = $(this).val();
				$('#@{heatmap_id}_heatmap_brush').css('border-width', val);
				$('#@{heatmap_id}_heatmap').mousedown(function() {
					if($('#@{heatmap_id}_heatmap_brush').length > 0) {
						$('#@{heatmap_id}_heatmap_brush').css('border-width', val);
					}
				});
			});
			$('#@{heatmap_id}_color_pickers_opacity').change(function() {
				var val = $(this).val();
				$('#@{heatmap_id}_heatmap_brush').css('opacity', val);
				$('#@{heatmap_id}_heatmap').mousedown(function() {
					if($('#@{heatmap_id}_heatmap_brush').length > 0) {
						$('#@{heatmap_id}_heatmap_brush').css('opacity', val);
					}
				});
			});
				")))
			)
		),
		tags$script(HTML(qq("
    $( '#@{heatmap_id}_tabs' ).tabs({
      collapsible: true,
      active: false
    });
    $('#@{heatmap_id}_tabs').tooltip({position: {my: 'center bottom-4', at: 'center top'}});
		")))
	)
}

heatmap_search_ui = function(heatmap_id, shiny_env) {
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
		p("Search is turned off because of no row/column labels.")
	} else {

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
		div(
			div(textInput(qq("@{heatmap_id}_keyword"), placeholder = "Multiple keywords separated by ','", label = "Keywords"), style = "width:250px;float:left;"),
			div(checkboxInput(qq("@{heatmap_id}_search_regexpr"), label = "Regular expression", value = FALSE), style = "width:150px;float:left;padding-top:20px;padding-left:4px;"),
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
	}
}

sub_heatmap_control_ui = function(heatmap_id) {

div(
	div(id = qq('@{heatmap_id}_sub_tabs'),
		HTML(qq("<ul>
			<li><a href='#@{heatmap_id}_sub_tabs-resize' title='Resize sub-heatmaps'><i class='fa fa-expand-arrows-alt'></i></a></li>
			<li><a href='#@{heatmap_id}_sub_tabs-save-image' title='Export sub-heatmaps as an image'><i class='fa fa-images'></i></a></li>
			<li><a href='#@{heatmap_id}_sub_tabs-table' title='Export table'><i class='fa fa-table'></i></a></li>
			<li><a href='#@{heatmap_id}_sub_tabs-setting' title='Configure sub-heatmaps'><i class='fa fa-tasks'></i></a></li>
		</ul>")),
		tags$style(qq("
#@{heatmap_id}_sub_tabs-setting .form-group {
	margin-bottom: 0px;
}
		")),
		div(id = qq('@{heatmap_id}_sub_tabs-setting'), 
			div(
				div(checkboxInput(qq("@{heatmap_id}_show_row_names_checkbox"), label = "Show row names", value = TRUE), style = "float:left;width:150px"),
				div(checkboxInput(qq("@{heatmap_id}_show_column_names_checkbox"), label = "Show column names", value = TRUE), style = "float:left;width:150px"),
				div(style = "clear: both;")
			),
			div(
				checkboxInput(qq("@{heatmap_id}_show_annotation_checkbox"), label = "Show heatmap annotations", value = TRUE),
				checkboxInput(qq("@{heatmap_id}_show_cell_fun_checkbox"), label = "Show cell decorations", value = TRUE)
			),
		),
		div(id = qq('@{heatmap_id}_sub_tabs-save-image'),
			radioButtons(qq("@{heatmap_id}_sub_heatmap_download_format"), label = "Which format?", choices = list("png" = 1, "pdf" = 2, "svg" = 3), selected = 1, inline = TRUE),
			numericInput(qq("@{heatmap_id}_sub_heatmap_download_image_width"), label = "Image width (in px)", value = 370),
			numericInput(qq("@{heatmap_id}_sub_heatmap_download_image_height"), label = "Image height (in px)", value = 350),
			downloadButton(qq("@{heatmap_id}_sub_heatmap_download_button"), "Save image")
		),
		div(id = qq('@{heatmap_id}_sub_tabs-resize'),
			numericInput(qq("@{heatmap_id}_sub_heatmap_input_width"), "Box width", 370),
			numericInput(qq("@{heatmap_id}_sub_heatmap_input_height"), "Box height", 350),
			actionButton(qq("@{heatmap_id}_sub_heatmap_input_size_button"), "Change image size"),
			tags$script(HTML(qq("
			$('#@{heatmap_id}_sub_heatmap_input_size_button').click(function(){
				var width = $('#@{heatmap_id}_sub_heatmap_input_width').val();
				var height = $('#@{heatmap_id}_sub_heatmap_input_height').val();
				$('#@{heatmap_id}_sub_heatmap_wrap').width(width+4);
				$('#@{heatmap_id}_sub_heatmap').width(width);
				$('#@{heatmap_id}_sub_heatmap img').width(width);
				$('#@{heatmap_id}_sub_heatmap_wrap').height(height+4);
				$('#@{heatmap_id}_sub_heatmap').height(height);
				$('#@{heatmap_id}_sub_heatmap img').height(height);
			});
			$('#@{heatmap_id}_sub_heatmap_download_format').change(function() {
				var width = $('#@{heatmap_id}_sub_heatmap_input_width').val();
				width = parseInt(width);
				var height = $('#@{heatmap_id}_sub_heatmap_input_height').val();
				height = parseInt(height);
				if(parseInt($(this).find('input').filter(':checked').val()) == 2) {
					width_in_inch = Math.round(width*10/100*4/3)/10;
					height_in_inch = Math.round(height*10/100*4/3)/10;
					$('#@{heatmap_id}_sub_heatmap_download_image_width').val(width_in_inch);
					$('#@{heatmap_id}_sub_heatmap_download_image_height').val(height_in_inch);
					$('#@{heatmap_id}_sub_heatmap_download_image_width').prev().text('Image width (in inch)');
					$('#@{heatmap_id}_sub_heatmap_download_image_height').prev().text('Image height (in inch)');
					Shiny.setInputValue('@{heatmap_id}_sub_heatmap_download_image_width', width_in_inch);
					Shiny.setInputValue('@{heatmap_id}_sub_heatmap_download_image_height', height_in_inch);
				} else {
					$('#@{heatmap_id}_sub_heatmap_download_image_width').val(width);
					$('#@{heatmap_id}_sub_heatmap_download_image_height').val(height);
					$('#@{heatmap_id}_sub_heatmap_download_image_width').prev().text('Image width (in px)');
					$('#@{heatmap_id}_sub_heatmap_download_image_height').prev().text('Image height (in px)');
				}
			});
			Shiny.setInputValue('@{heatmap_id}_sub_heatmap_download_trigger', Math.random());
			")))
		),
		div(id = qq("@{heatmap_id}_sub_tabs-table"),
			p("Export values in sub-heatmaps as a text table."),
			actionButton(qq("@{heatmap_id}_open_table"), label = "Open table")
		)
		
	),
	tags$script(HTML(qq("
$( '#@{heatmap_id}_sub_tabs' ).tabs({
	collapsible: true,
	active: false
});
$('#@{heatmap_id}_sub_tabs').tooltip({position: {my: 'center bottom-4', at: 'center top'}});
	")))

	)
	
}
