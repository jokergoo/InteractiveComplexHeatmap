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


# == title (data:rand_mat)
# A random matrix
#
# == details
# Following code was used to generate ``rand_mat``:
#
#   set.seed(123)
#   rand_mat = cbind(rbind(matrix(rnorm(20*20, mean = 1, sd = 0.5), nr = 20),
#                   matrix(rnorm(20*20, mean = 0, sd = 0.5), nr = 20),
#                   matrix(rnorm(20*20, mean = 0, sd = 0.5), nr = 20)),
#             rbind(matrix(rnorm(20*20, mean = 0, sd = 0.5), nr = 20),
#                   matrix(rnorm(20*20, mean = 1, sd = 0.5), nr = 20),
#                   matrix(rnorm(20*20, mean = 0, sd = 0.5), nr = 20)),
#             rbind(matrix(rnorm(20*20, mean = 0.5, sd = 0.5), nr = 20),
#                   matrix(rnorm(20*20, mean = 0.5, sd = 0.5), nr = 20),
#                   matrix(rnorm(20*20, mean = 1, sd = 0.5), nr = 20))
#            ) + matrix(rnorm(60*60, sd = 0.5), nr = 60)
#    colnames(rand_mat) = paste0("C", 1:60)
#    rownames(rand_mat) = paste0("R", 1:60)
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# data(rand_mat)
# rand_mat