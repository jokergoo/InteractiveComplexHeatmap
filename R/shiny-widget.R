
# == title
# Interactive complex heatmap modal
#
# == param
# -input Passed from the shiny server function.
# -output Passed from the shiny server function.
# -session Passed from the shiny server function.
# -get_heatmap A `ComplexHeatmap::Heatmap-class` or a `ComplexHeatmap::HeatmapList-class` object. The value can also
#           be a function with no argument that generates such object.
# -heatmap_id ID of the plot. If it is not specified, an internal ID is assigned.
# -title1 Pass to `InteractiveComplexHeatmapOutput`.
# -title2 Pass to `InteractiveComplexHeatmapOutput`.
# -width1 Pass to `InteractiveComplexHeatmapOutput`.
# -height1 Pass to `InteractiveComplexHeatmapOutput`.
# -width2 Pass to `InteractiveComplexHeatmapOutput`.
# -height2 Pass to `InteractiveComplexHeatmapOutput`.
# -nrow Pass to `InteractiveComplexHeatmapOutput`.
# -action Pass to `InteractiveComplexHeatmapOutput`.
# -brush_opt Pass to `InteractiveComplexHeatmapOutput`.
# -output_div Pass to `InteractiveComplexHeatmapOutput`.
# -click_action Pass to `renderInteractiveComplexHeatmap`.
# -brush_action Pass to `renderInteractiveComplexHeatmap`.
# -default_click_action Pass to `renderInteractiveComplexHeatmap`.
# -default_brush_action Pass to `renderInteractiveComplexHeatmap`.
# -js_code Additional javascript code that is put after the interactive heatmap UI. The value can be a text or a function
#       that takes "heatmap id" as the argument and returns the formatted javascript code.
# -close_button Whether to add a close button at the end of the widget. If it is ``FALSE``, the widget
#      can be closed by click outside of the widget.
#
# == details
# It create an interactive heatmap "modal" according to a certain action.
#
# == value
# No value is returned.
#
# == example
# if(interactive()) {
#     require(ComplexHeatmap)
#     
#     ui = fluidPage(
#         actionButton("show_heatmap", "Generate_heatmap"),
#     )
#     
#     server = function(input, output, session) {
#         m = matrix(rnorm(100), 10)
#         ht = Heatmap(m)
#         
#         observeEvent(input$show_heatmap, {
#             InteractiveComplexHeatmapModal(input, output, session, ht)
#         })
#     }
#     shiny::shinyApp(ui, server)
# }
InteractiveComplexHeatmapModal = function(
	input, output, session, get_heatmap, heatmap_id = NULL,

	# parameters passed to InteractiveComplexHeatmapOutput()
	title1 = "Original heatmap", title2 = "Selected sub-heatmap",
	width1 = 450, height1 = 350, width2 = 370, height2 = 350, nrow = 1,
	action = "click", brush_opt = list(), output_div = TRUE,

	# parameters passed to renderInteractiveComplexHeatmap()
	click_action = NULL, brush_action = NULL, 
	default_click_action = TRUE, default_brush_action = TRUE,

	# other configurations
	js_code = "", close_button = FALSE, cancel_action = c("remove", "hide")
	) {
	
	if(is.null(heatmap_id)) {
		increase_widget_index()
		heatmap_id = paste0("ht", get_widget_index())
	}

	insertUI(selector = qq("body"), 
		where = "beforeEnd",
		ui = htmlOutput(qq("@{heatmap_id}_heatmap_modal_ui")))
	
	if(is.function(js_code)) js_code = js_code(heatmap_id)
	js_code = paste(js_code, collapse = "\n")

	cancel_action = match.arg(cancel_action)[1]

	output[[qq("@{heatmap_id}_heatmap_modal_ui")]] = renderUI({
		div(id = qq("@{heatmap_id}_heatmap_modal_background"),
			div(id = qq("@{heatmap_id}_heatmap_modal"),
				InteractiveComplexHeatmapOutput(heatmap_id = heatmap_id, title1 = title1, title2 = title2,
					width1 = width1, height1 = height1, width2 = width2, height2 = height2, nrow = nrow,
					action = action, brush_opt = brush_opt, output_div = output_div),
				if(close_button) {
					tagList(
						tags$hr(),
						tags$button("Close widget", id = qq("@{heatmap_id}_heatmap_modal_close"), class="btn btn-default")
					)
				} else {
					NULL
				},
				tags$script(qq("
					Shiny.setInputValue('@{heatmap_id}_heatmap_modal_open', Math.random());
				"))
			),
			tags$style(HTML(qq("
				#@{heatmap_id}_heatmap_modal_background {
					left: 0;
					right: 0;
					top: 0;
					bottom: 0;
					position: fixed;
					background-color: rgb(0, 0, 0, 0.5);
					overflow-y: auto;
				}
				#@{heatmap_id}_heatmap_modal {
					position: relative;
					margin: 50px;
					padding: 20px;
				    background-color: #fff;
				    background-clip: padding-box;
					transition: opacity .15s linear;
					box-shadow: 0 5px 15px rgba(0,0,0,.25);
					border: 1px solid rgba(0,0,0,.2);
					border-radius: 6px;
					outline: 0;
				}
			"))),
			if(close_button) {
				tags$script(HTML(qq("
					$('#@{heatmap_id}_heatmap_modal_close').click(function() {
	
				        if('@{cancel_action}' == 'remove') {
				        	Shiny.setInputValue('@{heatmap_id}_heatmap_modal_remove', Math.random());
				        	$('#@{heatmap_id}_heatmap_modal_ui').remove();
				        } else {
				        	$('#@{heatmap_id}_heatmap_modal_background').@{cancel_action}();
				        }
					})
				")))
			} else {
				tags$script(HTML(qq("
					$(document).mouseup(function(e) {
					    var container = $('#@{heatmap_id}_heatmap_modal');

					    if(!container.is(e.target) && container.has(e.target).length === 0) {
					        $('#@{heatmap_id}_heatmap_modal_background').@{cancel_action}();
					        if('@{cancel_action}' == 'remove') {
					        	Shiny.setInputValue('@{heatmap_id}_heatmap_modal_remove', Math.random());
					        	$('#@{heatmap_id}_heatmap_modal_ui').remove();
					        }
					    }
					});

					@{js_code}
				")))
			},
			tags$script(HTML(qq("
				// code for pickr
				// when parent element has 'position:fixed', the color picker is not correctly positioned
				// following code manually adjust the positions of the color picker
				$($('#@{heatmap_id}_heatmap_control ul li')[1]).click(function() {
					var @{heatmap_id}_color_picker_buttons = $('#@{heatmap_id}_tabs-brush button.pcr-button');
						
					$(@{heatmap_id}_color_picker_buttons[0]).click(function() {
						var offset = $(@{heatmap_id}_color_picker_buttons[0]).offset();
						var w = $(@{heatmap_id}_color_picker_buttons[0]).outerWidth();
						var h = $(@{heatmap_id}_color_picker_buttons[0]).outerHeight();
						$('#@{heatmap_id}_tabs-brush .pcr-app.visible').css('top', offset.top + h + 5).
						                                                css('left', offset.left);
					})

					$(@{heatmap_id}_color_picker_buttons[1]).click(function() {
						var offset = $(@{heatmap_id}_color_picker_buttons[1]).offset();
						var w = $(@{heatmap_id}_color_picker_buttons[1]).outerWidth();
						var h = $(@{heatmap_id}_color_picker_buttons[1]).outerHeight();
						$('#@{heatmap_id}_tabs-brush .pcr-app.visible').css('top', offset.top + h + 5).
						                                                css('left', offset.left);
					})
				})
			")))
		)
	})

	observeEvent(input[[qq("@{heatmap_id}_heatmap_modal_open")]], {
		if(is.function(get_heatmap)) {
			ht = get_heatmap()
		} else {
			ht = get_heatmap
		}
		renderInteractiveComplexHeatmap(input, output, session, ht, heatmap_id = heatmap_id,
			click_action = click_action, brush_action = brush_action,
			default_click_action = default_click_action, default_brush_action = default_brush_action)
	})

	observeEvent(input[[qq("@{heatmap_id}_heatmap_modal_remove")]], {
		removeUI(qq("#@{heatmap_id}_heatmap_modal_background"))
	})
}

# == title
# Interactive complex heatmap widget
#
# == param
# -input Passed from the shiny server function.
# -output Passed from the shiny server function.
# -session Passed from the shiny server function.
# -get_heatmap A `ComplexHeatmap::Heatmap-class` or a `ComplexHeatmap::HeatmapList-class` object. The value can also
#           be a function with no argument that generates such object.
# -heatmap_id ID of the plot. If it is not specified, an internal ID is assigned.
# -output_id Where the heatmap is put.
# -title1 Pass to `InteractiveComplexHeatmapOutput`.
# -title2 Pass to `InteractiveComplexHeatmapOutput`.
# -width1 Pass to `InteractiveComplexHeatmapOutput`.
# -height1 Pass to `InteractiveComplexHeatmapOutput`.
# -width2 Pass to `InteractiveComplexHeatmapOutput`.
# -height2 Pass to `InteractiveComplexHeatmapOutput`.
# -nrow Pass to `InteractiveComplexHeatmapOutput`.
# -action Pass to `InteractiveComplexHeatmapOutput`.
# -brush_opt Pass to `InteractiveComplexHeatmapOutput`.
# -output_div Pass to `InteractiveComplexHeatmapOutput`.
# -click_action Pass to `renderInteractiveComplexHeatmap`.
# -brush_action Pass to `renderInteractiveComplexHeatmap`.
# -default_click_action Pass to `renderInteractiveComplexHeatmap`.
# -default_brush_action Pass to `renderInteractiveComplexHeatmap`.
# -js_code Additional javascript code that is put after the interactive heatmap UI. The value can be a text or a function
#       that takes "heatmap id" as the argument and returns the formatted javascript code.
# -close_button Whether to add a close button at the end of the widget.
#
# == details
# It create an interactive heatmap widget according to a certain action. The UI is fit to the output ID user defined.
#
# == value
# No value is returned.
#
# == example
# if(interactive()) {
#     require(ComplexHeatmap)
#     
#     ui = fluidPage(
#         actionButton("show_heatmap", "Generate_heatmap"),
#         htmlOutput("heatmap_output")
#     )
#     
#     server = function(input, output, session) {
#         m = matrix(rnorm(100), 10)
#         ht = Heatmap(m)
#         
#         observeEvent(input$show_heatmap, {
#             InteractiveComplexHeatmapWidget(input, output, session, ht, 
#                 output_id = "heatmap_output")
#         })
#     }
#     shiny::shinyApp(ui, server)
# }
InteractiveComplexHeatmapWidget = function(
	input, output, session, get_heatmap, heatmap_id = NULL, output_id,

	# parameters passed to InteractiveComplexHeatmapOutput()
	title1 = "Original heatmap", title2 = "Selected sub-heatmap",
	width1 = 450, height1 = 350, width2 = 370, height2 = 350, nrow = 1,
	action = "click", brush_opt = list(), output_div = TRUE,

	# parameters passed to renderInteractiveComplexHeatmap()
	click_action = NULL, brush_action = NULL, 
	default_click_action = TRUE, default_brush_action = TRUE,

	# other configurations
	js_code = "", close_button = FALSE, cancel_action = c("remove", "hide")
	) {
	
	if(is.null(heatmap_id)) {
		increase_widget_index()
		heatmap_id = paste0("ht", get_widget_index())
	}

	if(is.function(js_code)) js_code = js_code(heatmap_id)
	js_code = paste(js_code, collapse = "\n")

	cancel_action = match.arg(cancel_action)[1]

	output[[output_id]] = renderUI({
		div(id = qq("@{heatmap_id}_container"),
			InteractiveComplexHeatmapOutput(heatmap_id = heatmap_id, title1 = title1, title2 = title2,
				width1 = width1, height1 = height1, width2 = width2, height2 = height2, nrow = nrow,
				action = action, brush_opt = brush_opt, output_div = output_div),
			if(close_button) {
				tagList(
					tags$hr(),
					tags$button("Close widget", id = qq("@{heatmap_id}_heatmap_modal_close"), 
						class = "btn btn-default"),
					tags$script(HTML(qq("
						$('#@{heatmap_id}_heatmap_modal_close').click(function() {
							$('#@{heatmap_id}_heatmap_modal_background').@{cancel_action}();
					        if('@{cancel_action}' == 'remove') {
					        	Shiny.setInputValue('@{heatmap_id}_heatmap_modal_remove', Math.random());
					        	$('#@{heatmap_id}_heatmap_modal_ui').remove();
					        }
						})
					")))
				)
			} else {
				NULL
			},
			tags$script(qq("Shiny.setInputValue('@{heatmap_id}_heatmap_modal_open', Math.random());
				@{js_code}
			"))
		)
	})
	
	observeEvent(input[[qq("@{heatmap_id}_heatmap_modal_open")]], {
		if(is.function(get_heatmap)) {
			ht = get_heatmap()
		} else {
			ht = get_heatmap
		}
		renderInteractiveComplexHeatmap(input, output, session, ht, heatmap_id = heatmap_id,
			click_action = click_action, brush_action = brush_action,
			default_click_action = default_click_action, default_brush_action = default_brush_action)
	})

	observeEvent(input[[qq("@{heatmap_id}_heatmap_modal_remove")]], {
		removeUI(qq("#@{heatmap_id}_heatmap_modal_background"))
	})
}
