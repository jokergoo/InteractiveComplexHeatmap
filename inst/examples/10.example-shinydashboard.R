# Work with shinydashboard

##########################################
# title: Seperate the three UI components into three boxes.

library(shinydashboard)
m = matrix(rnorm(100), 10)
ht = Heatmap(m)

body = dashboardBody(
	fluidRow(
		box(
			title = "Original heatmap", width = 4, solidHeader = TRUE, status = "primary",
			mainHeatmapOutput("ht", title = NULL)
		),
		box(
			title = "Sub-heatmap", width = 4, solidHeader = TRUE, status = "primary",
			subHeatmapOutput("ht", title = NULL)
		),
		box(
			title = "Output", width = 4, solidHeader = TRUE, status = "primary",
			HeatmapInfoOutput("ht", title = NULL)
		)
	)
)

ui = dashboardPage(
	dashboardHeader(title = "InteractiveComplexHeatmap works with shinydashboard"),
	dashboardSidebar(),
	body
)

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht, "ht")
}

shinyApp(ui, server)


##########################################
# title: A Shiny dashboard with two tabs

library(shinydashboard)
m1 = matrix(rnorm(100), 10)
ht1 = Heatmap(m1)

m2 = matrix(sample(letters[1:10], 100, replace = TRUE), 10)
ht2 = Heatmap(m2)

side_bar = dashboardSidebar(
	sidebarMenu(
		menuItem("A numeric heatmap", tabName = "numeric"),
		menuItem("A character heatmap", tabName = "character")
	)
)

single_heatmap_ui = function(heatmap_id) {
	fluidRow(
		box(
			title = "Original heatmap", width = 4, solidHeader = TRUE, status = "primary",
			mainHeatmapOutput(heatmap_id, title = NULL)
		),
		box(
			title = "Sub-heatmap", width = 4, solidHeader = TRUE, status = "primary",
			subHeatmapOutput(heatmap_id, title = NULL)
		),
		box(
			title = "Output", width = 4, solidHeader = TRUE, status = "primary",
			HeatmapInfoOutput(heatmap_id, title = NULL)
		)
	)
}

body = dashboardBody(
	tabItems(
		tabItem(tabName = "numeric", single_heatmap_ui("ht1")),
		tabItem(tabName = "character", single_heatmap_ui("ht2"))
	)
)

ui = dashboardPage(
	dashboardHeader(title = "InteractiveComplexHeatmap works with shinydashboard"),
	side_bar,
	body
)

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht1, "ht1")
    makeInteractiveComplexHeatmap(input, output, session, ht2, "ht2")
}

shinyApp(ui, server)


########################################################
# title: Only contains the main heatmap where output is floating

library(shinydashboard)
m = matrix(rnorm(100), 10)
ht = Heatmap(m)

body = dashboardBody(
	fluidRow(
		box(
			title = "Original heatmap", width = 4, solidHeader = TRUE, status = "primary",
			mainHeatmapOutput("ht", title = NULL, response = "click"),
			HeatmapInfoOutput("ht", title = NULL, output_ui_float = TRUE)
		)
	)
)

ui = dashboardPage(
	dashboardHeader(title = "InteractiveComplexHeatmap works with shinydashboard"),
	dashboardSidebar(),
	body
)

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht, "ht")
}

shinyApp(ui, server)
