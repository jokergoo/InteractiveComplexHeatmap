
ui = fluidPage(
	plotOutput("plot", click = "plot_click", brush = "plot_brush"),
	verbatimTextOutput("output_click"),
	verbatimTextOutput("output_brush"),
	actionButton("action", "reset"),
	tags$head(tags$script(HTML("
	var click_in_brush = 0;
	
	Shiny.addCustomMessageHandler('shortly_wait', function(message) {
		click_in_brush = 0;
		$.ajax({
			url: 'shared/shiny.css', 
			success: function(result) {
				setTimeout(function(){1;}, 1000);
				if(click_in_brush == 0) {
					Shiny.setInputValue('click_in_brush', false);
					Shiny.setInputValue('click_observe', Math.random());
				}
				click_in_brush == 0;
			},
			error: function(result) {
				setTimeout(function(){1;}, 1000);
				if(click_in_brush == 0) {
					Shiny.setInputValue('click_in_brush', false);
					Shiny.setInputValue('click_observe', Math.random());
				}
				click_in_brush == 0;
			}
		});
	});
	Shiny.addCustomMessageHandler('click_in_brush', function(message) {
		Shiny.setInputValue('click_in_brush', true);
		Shiny.setInputValue('click_observe', Math.random());
		click_in_brush = 1;
	});")))

)

server = function(input, output, session) {
	output$plot = renderPlot({
		plot(1)
	})

	observeEvent(input$plot_click, {
		session$sendCustomMessage("shortly_wait", "")
	})

	observeEvent(input$click_observe, {
browser()
		if(!input$click_in_brush) {
			output$output_click = renderText({
				"clicked"
			})
		}
	})

	observeEvent(input$plot_brush, {
		session$sendCustomMessage("click_in_brush", '');
		output$output_brush = renderText({
			"brushed"
		})
	})

	observeEvent(input$action, {
		output$output_click = renderText({
			""
		})
		output$output_brush = renderText({
			""
		})
	})
}

shinyApp(ui = ui, server = server)
