get_last_ht = function() {
	ComplexHeatmap:::.ENV$last
}


shiny_env = new.env()
shiny_env$i_widget = 0

get_widget_index = function() {
	shiny_env$i_widget
}

increase_widget_index = function() {
	shiny_env$i_widget = shiny_env$i_widget + 1
}
