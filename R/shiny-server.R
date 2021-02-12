
# == title
# Process the heatmaps on the sever side
#
# == param
# -input Passed from the Shiny server function.
# -output Passed from the Shiny server function.
# -session Passed from the Shiny server function.
# -ht_list A `ComplexHeatmap::Heatmap-class` or a `ComplexHeatmap::HeatmapList-class` object.
# -heatmap_id The corresponding heatmap ID from the UI. If there is only one interactive heatmap in the app, 
#     this argument does not need to be specified and it will use the current one specified in `InteractiveComplexHeatmapOutput`.
# -click_action Additional action at the sever side when receiving a click event on the UI. If ``action`` is selected as ``hover``
#        or ``dblclick`` in `InteractiveComplexHeatmapOutput`, then this argument controls the action for the hover or dblclick event.
# -brush_action Additional action at the sever side when receiving a brush event on the UI.
#
# == value
# No value is returned.
#
# == examples
# if(interactive()) {
#     ht = Heatmap(m)
#     ht = draw(ht)
#     
#     ui = fluidPage(
#         InteractiveComplexHeatmapOutput()
#     )
#     
#     server = function(input, output, session) {
#         makeInteractiveComplexHeatmap(input, output, session, ht)
#     }
#     
#     shiny::shinyApp(ui, server)
# }
makeInteractiveComplexHeatmap = function(input, output, session, ht_list, 
	heatmap_id = shiny_env$current_heatmap_id,
	click_action = NULL, brush_action = NULL) {

	do_default_click_action = shiny_env[[heatmap_id]]$default_output_ui
	do_default_brush_action = shiny_env[[heatmap_id]]$default_output_ui

	if(inherits(ht_list, "Heatmap")) {
		message("The heatmap is suggested to be updated by e.g. `ht = draw(ht)` before sending to the Shiny app.")
	} else if(inherits(ht_list, "HeatmapList")) {
		if(!ht_list@layout$initialized) {
			message("The heatmap list is suggested to be udpated by e.g. `ht_list = draw(ht_list)` before sending to the Shiny app.")
		}
	} else {
		stop_wrap("`ht_list` can only be a Heatmap/HeatmapList object.")
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

	# initialize heatmaps
	ht_list = reactiveVal({
		tryCatch({
			dev.null()
			if(inherits(ht_list, "Heatmap")) {
	    		ht_list = make_layout(ht_list + NULL)
	    	} else {
	    		if(!ht_list@layout$initialized) {
	    			ht_list = make_layout(ht_list)
	    		}
	    	}
	    }, finally = dev.off2())
	    
    	ht_list
	})

	##### variables shared between actions
	ht_pos = reactiveVal(NULL)
	selected = reactiveVal(NULL)
	selected_copy = reactiveVal(NULL)
	heatmap_first_check = reactiveVal(1)
	heatmap_initialized = reactiveVal(1)

	sub_ht_list = reactiveVal()

	###############################################################
	##                 The default actions
	###############################################################
	output[[qq("@{heatmap_id}_heatmap")]] = renderPlot({
    	showNotification("Initialize the original heatmap.", duration = 2, type = "message")

    	ht_list( draw(ht_list()) )

		ht_pos( htPositionsOnDevice(ht_list(), include_annotation = TRUE, calibrate = FALSE) )
		selected( NULL )

		if(do_default_click_action || do_default_brush_action) {
			output[[qq("@{heatmap_id}_info")]] = renderUI({
				HTML("<h5>Output</h5>\n<p>No position is selected.</p>")
			})
		}
		message(qq("[@{Sys.time()}] initialize the original heatmap and calculate positions."))

		lt = check_heatmap_in_search(heatmap_id, ht_list())

		if(is.null(lt)) {
			session$sendCustomMessage(qq("@{heatmap_id}_empty_search"), "")
		} else {
			updateRadioButtons(session, qq("@{heatmap_id}_search_where"), label = "Which dimension to search?", choices = lt[[1]], selected = lt[[1]][[1]], inline = TRUE)
			updateCheckboxGroupInput(session, qq("@{heatmap_id}_search_heatmaps"), label = "Which heatmaps to search?", choiceNames = lt[[2]], choiceValues = lt[[2]], selected = lt[[2]])
		}
		session$sendCustomMessage(qq("@{heatmap_id}_initialized"), "")

		width_div = session$clientData[[qq("output_@{heatmap_id}_heatmap_width")]]
	    height_div = session$clientData[[qq("output_@{heatmap_id}_heatmap_height")]]
	    
	    width_ht = ComplexHeatmap:::width(ht_list())	
	    height_ht = ComplexHeatmap:::height(ht_list())

	    df = ht_pos()
	    df = df[!is.na(df$row_slice), , drop = FALSE]
	    x_min = df$x_min; x_min = convertX(x_min, "bigpts", valueOnly = TRUE)
	    x_max = df$x_max; x_max = convertX(x_max, "bigpts", valueOnly = TRUE)
	    y_min = df$y_min; y_min = convertY(y_min, "bigpts", valueOnly = TRUE)
	    y_max = df$y_max; y_max = convertY(y_max, "bigpts", valueOnly = TRUE)

	    warning_msg = ""
	    if(any(x_min < 0) || any(x_max > width_div) || any(y_min < 0) || any(y_max > height_div)) {
	    	warning_msg = qq("Heatmaps exceed the figure region")
	    	if(is_abs_unit(width_ht) && is_abs_unit(height_ht)) {
	    		width_ht = ceiling(convertWidth(width_ht, "bigpts", valueOnly = TRUE))
	    		height_ht = ceiling(convertHeight(height_ht, "bigpts", valueOnly = TRUE))
	    		warning_msg = qq("@{warning_msg} because the heatmaps have widths and heights in absolute units and the figure size is too small to fully contain them. You can set the width and height as <code>htShiny(..., width1 = @{width_ht}, height1 = @{height_ht})</code>, or similarly in <code>InteractiveComplexHeatmapOutput()</code> or other related functions.")
	    	} else if(is_abs_unit(width_ht)) {
	    		width_ht = ceiling(convertWidth(width_ht, "bigpts", valueOnly = TRUE))
	    		warning_msg = qq("@{warning_msg} because the heatmaps have widths in absolute units and the figure size is too small to fully contain them. You can set the width as <code>htShiny(..., width1 = @{width_ht})</code>, or similarly in <code>InteractiveComplexHeatmapOutput()</code> or other related functions.")
	    	} else if(is_abs_unit(height_ht)) {
	    		height_ht = ceiling(convertHeight(height_ht, "bigpts", valueOnly = TRUE))
	    		warning_msg = qq("@{warning_msg} because the heatmaps have heights in absolute units and the figure size is too small to fully contain them. You can set the height as <code>htShiny(..., height1 = @{height_ht})</code>, or similarly in <code>InteractiveComplexHeatmapOutput()</code> or other related functions.")
	    	}

	    	output[[qq("@{heatmap_id}_warning")]] = renderUI({
	    		div(id = qq("@{heatmap_id}_warning_content"),
	    			h5("Warning"),
	    			p(HTML(warning_msg)),
	    			p(HTML(qq("<a href='#' onclick='$(\"#@{heatmap_id}_warning_content\").remove();false;'>Close</a>")),
	    				style = "position:relative; right:0; top:0"),
	    			style = "border: 1px solid red; border-radius: 4px; background-color:#FFDDDD; padding:5px 5px 2px 20px; max-width:850px",
	    		)
	    	})
	    }

		heatmap_first_check(0)

	})

	output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
		grid.newpage()
		grid.text("No area on the heatmap is selected.", 0.5, 0.5, gp = gpar(fontsize = 14))

		message(qq("[@{Sys.time()}] no area on the heatmap is selected, Do not make the sub-heatmap."))
	})

	if(do_default_click_action || do_default_brush_action) {
		output[[qq("@{heatmap_id}_info")]] = renderUI({
			HTML("<h5>Output</h5>\n<p>No position is selected.</p>")
		})
	}

	###############################################################
	##                 resizing
	###############################################################
	observeEvent(
		session$clientData[[qq("output_@{heatmap_id}_heatmap_width")]] || 
		session$clientData[[qq("output_@{heatmap_id}_heatmap_height")]], {

		req(!heatmap_first_check())

		output[[qq("@{heatmap_id}_heatmap")]] = renderPlot({
			width = session$clientData[[qq("output_@{heatmap_id}_heatmap_width")]]
	    	height = session$clientData[[qq("output_@{heatmap_id}_heatmap_height")]]
	    	
	    	showNotification("Making the original heatmap.", duration = 2, type = "message")

	    	draw(ht_list())

			ht_pos( htPositionsOnDevice(ht_list(), include_annotation = TRUE, calibrate = FALSE) )
			selected( NULL )

			message(qq("[@{Sys.time()}] make the original heatmap and calculate positions (device size: @{width}x@{height} px)."))
			
		})
		
	})

	###############################################################
	##                 The original heatmap
	###############################################################
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
			} else if(fm == "pdf") {
				dev(temp, width = width/100*4/3, height = height/100*4/3)
			} else {
				dev(temp, width = width, height = height)
			}
			if(heatmap_initialized()) {
		    	draw(ht_list())
		    } else {
		    	grid.newpage()
		    	grid.text("No heatmap is available.")
		    }
		    dev.off()

			file.copy(temp, file)
		}
	)
	

	observeEvent(input[[qq("@{heatmap_id}_heatmap_input_size_button")]], {

		req(heatmap_initialized())

		output[[qq("@{heatmap_id}_heatmap")]] = renderPlot({
			width = input[[qq("@{heatmap_id}_heatmap_input_width")]]
	    	height = input[[qq("@{heatmap_id}_heatmap_input_height")]]
	    	
	    	draw( ht_list() )

	    	showNotification(qq("Resizing the original heatmap (device size: @{width}x@{height} px)."), duration = 2, type = "message")
	    })
		
	})

	observeEvent(input[[qq("@{heatmap_id}_heatmap_brush")]], {

		if(is.null(input[[qq("@{heatmap_id}_heatmap_brush")]])) {
			selected( NULL )
			selected_copy( selected() )
		} else {
			lt = get_pos_from_brush(input[[qq("@{heatmap_id}_heatmap_brush")]])
		  	pos1 = lt[[1]]
		  	pos2 = lt[[2]]
		    
		    dev.null()
		    selected( selectArea(ht_list(), mark = FALSE, pos1 = pos1, pos2 = pos2, verbose = FALSE, ht_pos = ht_pos(), include_annotation = TRUE, calibrate = FALSE) )
		    selected_copy( selected() )
		    dev.off2()
		}

		updateTextInput(session, qq("@{heatmap_id}_keyword"), value = "")

		output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
			
    		if(is.null( selected() )) {
    			grid.newpage()
				grid.text("No area on the heatmap is selected.", 0.5, 0.5, gp = gpar(fontsize = 14))
    		} else {
    			sub_ht_list( make_sub_heatmap(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list()) )
			}
		})
	
		if(do_default_brush_action) {
			default_brush_action(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list())
		}

		if(!is.null(brush_action)) {
			if(identical(brush_action, default_brush_action)) {
				default_brush_action(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list())
			} else {
				brush_action(selected(), output)
			}
		}

		session$sendCustomMessage(qq("@{heatmap_id}_sub_initialized"), "on")
	})

	observeEvent(input[[qq("@{heatmap_id}_post_remove_submit")]], {

		new_selected = adjust_df(selected(), n_remove = input[[qq("@{heatmap_id}_post_remove")]], 
			where = input[[qq("@{heatmap_id}_post_remove_dimension")]])

		selected(new_selected)

		output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
			
    		if(nrow( selected() ) == 0) {
    			grid.newpage()
				grid.text("No row/column is left.\nPlease change to a smaller number to remove.", 0.5, 0.5, gp = gpar(fontsize = 14))
    		} else {
    			sub_ht_list( make_sub_heatmap(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list()) )
			}
		})
	
		if(do_default_brush_action) {
			default_brush_action(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list())
		}

		if(!is.null(brush_action)) {
			if(identical(brush_action, default_brush_action)) {
				default_brush_action(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list())
			} else {
				brush_action(selected(), output)
			}
		}

		session$sendCustomMessage(qq("@{heatmap_id}_sub_initialized"), "on")
	})

	observeEvent(input[[qq("@{heatmap_id}_post_remove_reset")]], {

		selected( selected_copy() )

		output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
			
    		if(is.null( selected() )) {
    			grid.newpage()
				grid.text("No area on the heatmap is selected.", 0.5, 0.5, gp = gpar(fontsize = 14))
    		} else {
    			sub_ht_list( make_sub_heatmap(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list()) )
			}
		})
	
		if(do_default_brush_action) {
			default_brush_action(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list())
		}

		if(!is.null(brush_action)) {
			if(identical(brush_action, default_brush_action)) {
				default_brush_action(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list())
			} else {
				brush_action(selected(), output)
			}
		}

		session$sendCustomMessage(qq("@{heatmap_id}_sub_initialized"), "on")
	})


	###############################################################
	##      sub-heatmap by selecting or searching
	###############################################################
	observeEvent(input[[qq("@{heatmap_id}_search_action")]], {
		if(input[[qq("@{heatmap_id}_keyword")]] == "") {
			output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
				grid.newpage()
				grid.text("Query keyword is empty.", 0.5, 0.5, gp = gpar(fontsize = 14, col = "red"))
			})

			if(do_default_brush_action) {
				default_brush_action(input, output, session, heatmap_id, "Query keyword is empty.", selected = selected(), ht_list = ht_list())
			}

			if(!is.null(brush_action)) {
				if(identical(brush_action, default_brush_action)) {
					default_brush_action(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list())
				} else {
					brush_action(selected(), output)
				}
			}

			session$sendCustomMessage(qq("@{heatmap_id}_sub_initialized"), "off")

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

			if(do_default_brush_action) {
				default_brush_action(input, output, session, heatmap_id, "No heatmap is selected for searching.", selected = selected(), ht_list = ht_list())
			}

			if(!is.null(brush_action)) {
				if(identical(brush_action, default_brush_action)) {
					default_brush_action(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list())
				} else {
					brush_action(selected(), output)
				}
			}
			return(invisible(NULL))
		}

		hl = ht_list()

		all_ht_name = sapply(hl@ht_list, function(x) {
			if(inherits(x, "Heatmap")) x@name else NA
		})
		all_ht_name = all_ht_name[!is.na(all_ht_name)]

		message(qq("[@{Sys.time()}] search heatmap @{ifelse(where == 1, 'row', 'column')}s with @{ifelse(is_regexpr, 'regular expression', 'keywords')}: '@{keywords}'."))

		if(!is_regexpr) {
			keywords = gsub("^\\s+||\\s+$", "", keywords)
			keywords = strsplit(keywords, "\\s*,\\s*")[[1]]
		}

		if(where == 1) {
			selected( selectByLabels(hl, row_keywords = keywords, keyword_is_regexpr = is_regexpr, include_annotation = TRUE, heatmap = sht, all = length(extend)) )
		} else if(where == 2) {
			selected( selectByLabels(hl, column_keywords = keywords, keyword_is_regexpr = is_regexpr, include_annotation = TRUE, heatmap = sht, all = length(extend)) )
		} else {
			selected( selectByLabels(hl, row_keywords = keywords, column_keywords = keywords, keyword_is_regexpr = is_regexpr, include_annotation = TRUE, heatmap = sht, all = length(extend)) )
		}
		selected_copy( selected() )

		output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
			
    		if(is.null(selected())) {
    			grid.newpage()
				grid.text(paste(strwrap(qq("Found nothing from heatmaps with keywords '@{keywords2}'."), width = 60), collapse = "\n"), 0.5, 0.5, gp = gpar(fontsize = 14, col = "red"))

				if(do_default_brush_action) {
					default_brush_action(input, output, session, heatmap_id, qq("Found nothing from heatmaps with keywords '@{keywords2}'."), selected = selected(), ht_list = ht_list())
				}

				if(!is.null(brush_action)) {
					brush_action(selected(), output)
				}
				return(invisible(NULL))
    		} else {
    			sub_ht_list( make_sub_heatmap(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list()) )
			}
		})

		if(do_default_brush_action) {
			default_brush_action(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list())
		}

		if(!is.null(brush_action)) {
			if(identical(brush_action, default_brush_action)) {
				default_brush_action(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list())
			} else {
				brush_action(selected(), output)
			}
		}

		session$sendCustomMessage(qq("@{heatmap_id}_sub_initialized"), "on")

	})

	output[[qq("@{heatmap_id}_sub_heatmap_download_button")]] = downloadHandler(

		filename = function() {
			format = as.numeric(input[[qq("@{heatmap_id}_sub_heatmap_download_format")]])
			fm = c("png", "pdf", "svg")[format]
			qq("@{heatmap_id}_sub_heatmap.@{fm}")
		},
		content = function(file) {
			
			format = as.numeric(input[[qq("@{heatmap_id}_sub_heatmap_download_format")]])
			fm = c("png", "pdf", "svg")[format]
			dev = list(png, pdf, svglite::svglite)[[format]]

			showNotification(qq("Download sub-heatmap in @{fm}."), duration = 2, type = "message")
			message(qq("[@{Sys.time()}] Download sub-heatmap in @{fm}."))

			temp = tempfile()
			width = input[[qq("@{heatmap_id}_sub_heatmap_download_image_width")]]
			height = input[[qq("@{heatmap_id}_sub_heatmap_download_image_height")]]
			
			if(fm == "png") {
				dev(temp, width = width*2, height = height*2, res = 72*2)
			} else if(fm == "pdf") {
				dev(temp, width = width/100*4/3, height = height/100*4/3)
			} else {
				dev(temp, width = width, height = height)
			}
			if(is.null(selected())) {
    			grid.newpage()
				grid.text("No heatmap is available.")
    		} else {
    			make_sub_heatmap(input, output, session, heatmap_id, newpage = FALSE, selected = selected(), ht_list = ht_list())
			}
		    dev.off()

			file.copy(temp, file)
		}
	)
	
	observeEvent(input[[qq("@{heatmap_id}_sub_heatmap_input_size_button")]], {
		
		output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
			if(is.null(selected())) {
    			grid.newpage()
				grid.text("No area on the heatmap is selected.", 0.5, 0.5, gp = gpar(fontsize = 14))
    		} else {
    			make_sub_heatmap(input, output, session, heatmap_id, update_size = FALSE, selected = selected(), ht_list = ht_list())
			}
		})
	})

	observeEvent(input[[qq("@{heatmap_id}_open_table")]], {
		if(is.null(selected())) {
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
				HTML(format_html_table(heatmap_id, selected = selected(), ht_list = ht_list()))
			})

		}
	})

	
	observeEvent(input[[qq("@{heatmap_id}_digits")]], {

		output[[qq("@{heatmap_id}_selected_table")]] = renderUI({
			HTML(format_html_table(heatmap_id, input[[qq("@{heatmap_id}_digits")]], selected = selected(), ht_list = ht_list()))
		})
	})

	output[[qq("@{heatmap_id}_download_table")]] = downloadHandler(
		filename = function() {
			qq("@{heatmap_id}_download_table.csv")
		},
		content = function(file) {
			tb = get_sub_matrix(heatmap_id, selected = selected(), ht_list = ht_list())
			write.table(tb, file, row.names = FALSE, col.names = FALSE, sep = ",", quote = TRUE)
		}
	)

	observeEvent(input[[qq("@{heatmap_id}_open_modal")]], {
		InteractiveComplexHeatmapModal(input, output, session, sub_ht_list(), close_button = TRUE)
	})

	###############################################################
	##      A click on the heatmap
	###############################################################
	observeEvent(input[[qq("@{heatmap_id}_heatmap_click")]], {
		pos1 = get_pos_from_click(input[[qq("@{heatmap_id}_heatmap_click")]])
		  
		if(is.null(pos1)) {
			selected( NULL )
		} else {
			dev.null()
			selected( selectPosition(ht_list(), mark = FALSE, pos = pos1, verbose = FALSE, ht_pos = ht_pos(), calibrate = FALSE) )
			dev.off2()
		}

		if(do_default_click_action) {
			default_click_action(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list())
		}

		if(!is.null(click_action)) {
			if(identical(click_action, default_click_action)) {
				default_click_action(input, output, session, heatmap_id, selected = selected(), ht_list = ht_list())
			} else {
				click_action(selected(), output)
			}
		}

		output[[qq("@{heatmap_id}_sub_heatmap")]] = renderPlot({
			grid.newpage()
			grid.text("No area on the heatmap is selected.", 0.5, 0.5, gp = gpar(fontsize = 14))
		})

		output[[qq("@{heatmap_id}_sub_heatmap_control")]] = renderUI({
			NULL
		})

		session$sendCustomMessage(qq("@{heatmap_id}_sub_initialized"), "off")

	})
}


# == title
# Process the heatmaps on the sever side
#
# == param
# -... All goes to `makeInteractiveComplexHeatmap`.
#
# == details
# The function ``renderInteractiveComplexHeatmap()`` was renamed to ``makeInteractiveComplexHeatmap``
# to get rid of the confusion of the use of ``render*()`` functions.
#
# == value
# No value is returned.
renderInteractiveComplexHeatmap = function(...) {
	makeInteractiveComplexHeatmap(...)
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

make_sub_heatmap = function(input, output, session, heatmap_id, update_size = TRUE, 
	selected = NULL, ht_list = NULL, ...) {
	showNotification("Making the selected sub-heatmap.", duration = 2, type = "message")

	width = session$clientData[[qq("output_@{heatmap_id}_sub_heatmap_width")]]
    height = session$clientData[[qq("output_@{heatmap_id}_sub_heatmap_height")]]

	show_row_names = input[[qq("@{heatmap_id}_show_row_names_checkbox")]]
	show_column_names = input[[qq("@{heatmap_id}_show_column_names_checkbox")]]
	show_annotation = input[[qq("@{heatmap_id}_show_annotation_checkbox")]]
	show_cell_fun = input[[qq("@{heatmap_id}_show_cell_fun_checkbox")]]
	fill_figure = input[[qq("@{heatmap_id}_fill_figure_checkbox")]]

	if(is.null(show_row_names)) show_row_names = TRUE
	if(is.null(show_column_names)) show_column_names = TRUE
	if(is.null(show_annotation)) show_annotation = TRUE
	if(is.null(show_cell_fun)) show_cell_fun = TRUE
	if(is.null(fill_figure)) fill_figure = FALSE

    if(is.null(selected)) {
    	grid.newpage()
		grid.text("Selected area should overlap to heatmap bodies", 0.5, 0.5, gp = gpar(fontsize = 14))
    } else {

    	all_ht_name = unique(selected$heatmap)

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
					if(!fill_figure) {
						if(is_abs_unit(body_width)) {
							body_width = body_width * (length(ci)/ncol(m))
						} 
					}
					heatmap_width = unit(1, "npc")
				}
				if(is_abs_unit(ht_current_full@matrix_param$height)) {
					body_height = ht_current_full@matrix_param$height
					if(!fill_figure) {
						if(is_abs_unit(body_height)) {
							body_height = body_height * (length(ri)/nrow(m))
						}
					}
					heatmap_height = unit(1, "npc")
				}

				if(fill_figure) {
					heatmap_width = unit(1, "npc")
					body_width = NULL
					heatmap_height = unit(1, "npc")
					body_height = NULL
				}
				
				ht_current = Heatmap(subm, rect_gp = ht_current_full@matrix_param$gp,
					row_split = rs, column_split = cs,
			    	col = ht_current_full@matrix_color_mapping,
			    	show_heatmap_legend = FALSE,
			    	cluster_rows = FALSE, 
			    	cluster_columns = FALSE,
					row_title = NULL, 
					column_title = NULL,
					border = ht_current_full@matrix_param$border,
					row_labels = row_labels, 
					column_labels = column_labels,
					show_row_names = show_row_names, 
					row_names_side = ht_current_full@row_names_param$side,
					show_column_names = show_column_names, 
					column_names_side = ht_current_full@column_names_param$side,
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
	    ht_select = draw(ht_select, save_last = FALSE, ...)
	    message(qq("[@{Sys.time()}] make the sub-heatmap (device size: @{width}x@{height} px)."))
	}

	if(update_size) {
		updateNumericInput(session, qq("@{heatmap_id}_sub_heatmap_input_width"), value = session$clientData[[qq("output_@{heatmap_id}_sub_heatmap_width")]])
		updateNumericInput(session, qq("@{heatmap_id}_sub_heatmap_input_height"), value = session$clientData[[qq("output_@{heatmap_id}_sub_heatmap_height")]])
	}

	return(ht_select)
}

# if annotation is included, top/bottom annotation are all put at the bottom of the matrix
get_sub_matrix = function(heatmap_id, digits = 2, include_annotation = TRUE, selected = NULL, ht_list = NULL) {
	message(qq("[@{Sys.time()}] fetch selected tables."))

	dev.null()
	on.exit(dev.off2())

	all_ht_name = unique(selected$heatmap)

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
	default_text = "Selected area should overlap to heatmap bodies.",
	selected = NULL, ht_list = NULL) {

	output[[qq("@{heatmap_id}_info")]] = renderUI({

		if(is.null(selected)) {
			HTML(qq("<h5>Output</h5>\n<p>@{default_text}</p>"))
		} else {

			selected = selected[!is.na(selected$row_slice), ]

			n_ht = length(unique(selected$heatmap))

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

			con = textConnection("dump_txt", "w")
			dump("selected_df", file = con)
			close(con)
			dump_txt = dump_txt[-1]
			dump_txt = paste(dump_txt, collapse = "\n")
			HTML(paste(
				  qq("<h5>Output</h5>\n<p>Selected over @{n_ht} heatmap@{ifelse(n_ht > 1, 's', '')} with @{nr} row@{ifelse(nr > 1, 's', '')} and @{nc} column@{ifelse(nc > 1, 's', '')}. Row and column indices can be obtained by copying following code:</p>"),
				  "<div>",
				  qq("<p><button id='@{heatmap_id}_show_code' class='btn btn-default'>show/hide code</button></p>"),
				  qq("<pre id='@{heatmap_id}_code'>"),
				  dump_txt,
				  "</pre>",
				  "</div>",
				  "<script>",
				  qq("$('#@{heatmap_id}_code').hide();"),
				  qq("$('#@{heatmap_id}_show_code').click(function(){ $('#@{heatmap_id}_code').toggle(); });"),
				  "</script>",
				  
				  sep = "\n"))
		}
	})
}

default_click_action = function(input, output, session, heatmap_id, selected = NULL, ht_list = NULL) {
	output[[qq("@{heatmap_id}_info")]] = renderUI({

	    if(is.null(selected)) {
	    	HTML("<h5>Output</h5>\n<p>No cell is selected.</p>")
	    } else {
	    	showNotification(qq("Click on the heatmap."), duration = 2, type = "message")
	    	pos = selected

			if(is.null(pos)) {
				HTML("<h5>Output</h5>\n<p>You did not click inside the heatmap.</p>")
			} else {
				ht_name = pos[1, "heatmap"]
				slice_name = pos[1, "slice"]

				ht = ht_list@ht_list[[ht_name]]
		
				row_index = pos[1, "row_index"][[1]]
			    column_index = pos[1, "column_index"][[1]]
			    m = ht@matrix
			    v = m[row_index, column_index]

			    if(is.null(ht@heatmap_param$oncoprint_env)) {
			    	col = map_to_colors(ht@matrix_color_mapping, v)
			    } else {
			    	col = "#FFFFFF00"
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
				
				html = qq("
<h5>Output</h5>
<div>
<p>Information of the clicked cell:</p>
<pre>
heatmap: @{ht_name}
heatmap slice: @{slice_name}
row index: @{row_index}
row label: @{row_label}
column index: @{column_index}
column_label: @{column_label}
value: @{v} <span style='background-color:@{col};width=10px;'>    </span></pre>")

				value_txt = NULL
				if(!is.null(ht@top_annotation)) {
					value_txt = c(value_txt, get_anno_value(ht@top_annotation, column_index))
				}
				if(!is.null(ht@bottom_annotation)) {
					value_txt = c(value_txt, get_anno_value(ht@bottom_annotation, column_index))
				}
				if(!is.null(ht@left_annotation)) {
					value_txt = c(value_txt, get_anno_value(ht@left_annotation, row_index))
				}
				if(!is.null(ht@right_annotation)) {
					value_txt = c(value_txt, get_anno_value(ht@right_annotation, row_index))
				}

				if(length(value_txt)) {
					html = qq("@{html}
<p>Information of the associated annotations:</p>
<pre>
@{paste(value_txt, collapse = '\n')}</pre>")
				}

				html = paste0(html, "</div>")

				HTML(html)
			}
		}
	})
}

get_anno_value = function(ha, ind) {
	fun_name = sapply(ha@anno_list, function(anno) anno@fun@fun_name)

	l = fun_name %in% c("anno_points", "anno_simple", "anno_lines", "anno_barplot")

	if(sum(l) > 0) {

		ha = ha[l]
		txt = NULL
		for(i in seq_len(length(ha))) {
			anno = ha@anno_list[[i]]
			x = anno@fun@var_env$value
			if(anno@fun@fun_name == "anno_simple") {
				cm = anno@color_mapping

				if(is.matrix(x)) {
					vstr = qq("@{paste0(x[ind, ], '')} <span style='background-color:@{map_to_colors(cm, x[ind, ])};width=10px;'>    </span>", collapse = FALSE)
					vstr = qq(vstr, collapse = ", ")
					txt[i] = qq("@{anon@name}: @{vstr}")
				} else {
					txt[i] = qq("@{anno@name}: @{paste0(x[ind], '')} <span style='background-color:@{map_to_colors(cm, x[ind])};width=10px;'>    </span>")
				}

			} else {
				if(is.matrix(x)) {
					txt[i] = qq("@{anon@name}: @{paste(x[ind, ], collapse = ', ')}")
				} else {
					txt[i] = qq("@{anno@name}: @{paste0(x[ind], '')}")
				}
			}

		}

		return(txt)
	} else {
		return(NULL)
	}
}


check_heatmap_in_search = function(heatmap_id, ht_list) {
	all_ht_name = sapply(ht_list@ht_list, function(x) {
		if(inherits(x, "Heatmap")) x@name else NA
	})
	all_ht_name = all_ht_name[!is.na(all_ht_name)]

	has_row_labels = sapply(ht_list@ht_list, function(x) {
		if(inherits(x, "Heatmap")) {
			!is.null(x@row_names_param$labels)
		} else {
			FALSE
		}
	})
	has_row_labels = has_row_labels[all_ht_name]
	has_column_labels = sapply(ht_list@ht_list, function(x) {
		if(inherits(x, "Heatmap")) {
			!is.null(x@column_names_param$labels)
		} else {
			FALSE
		}
	})
	has_column_labels = has_column_labels[all_ht_name]
	if(!any(has_row_labels) && !any(has_column_labels)) {
		return(NULL)
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
		heatmaps_to_search = unname(heatmaps_to_search)

		return(list(where_choices, heatmaps_to_search))
	}
}


format_html_table = function(heatmap_id, digits = 2, selected = NULL, ht_list = NULL) {
	tb = get_sub_matrix(heatmap_id, digits = round(digits), selected = selected, ht_list = ht_list)
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

