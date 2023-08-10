get_last_ht = function() {
	ComplexHeatmap:::.ENV$last
}


shiny_env = new.env()
shiny_env$i_widget = 0
shiny_env$i_obs = 0
shiny_env$heatmap = list()
shiny_env$obs = list()
shiny_env$action_button_count = list()
shiny_env$initialized = list()

get_widget_index = function() {
	shiny_env$i_widget
}

increase_widget_index = function() {
	shiny_env$i_widget = shiny_env$i_widget + 1
}

# == title
# Get all row indicies from the selected data frame
#
# == param
# -df The selected data frame.
#
all_row_indices = function(df) {
	unique(unlist(df$row_index))
}

# == title
# Get all column indicies from the selected data frame
#
# == param
# -df The selected data frame.
#
all_column_indices = function(df) {
	unique(unlist(df$column_index))
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



check_pkg = function(pkg) {
	if(requireNamespace(pkg, quietly = TRUE)) {
		return(NULL)
	} else {

		if(!interactive()) {
			stop_wrap(qq("You need to manually install package '@{pkg}' from CRAN/Bioconductor."))
		}

		answer = readline(qq("Package '@{pkg}' is required but not installed. Do you want to install it? [y|n] "))

		if(tolower(answer) %in% c("y", "yes")) {
			if(!requireNamespace("BiocManager", quietly = TRUE)) {
				install.packages("BiocManager")
			}
			BiocManager::install(pkg)
		} else {
			stop_wrap(qq("You need to manually install package '@{pkg}' from CRAN/Bioconductor."))
		}
	}
}


js_has_brush = function(response) {
	l = "brush" %in% response
	ifelse(l, "true", "false")
}

js_has_hover = js_has_dblclick = js_has_click = function(response) {
	l = "click" %in% response
	ifelse(l, "true", "false")
}


guess_best_km = function(mat, max_km = 10) {
    wss = NA
    max_km = min(c(nrow(mat) - 1, max_km))
    for (i in 2:max_km) {
        oe = try(fit <- kmeans(mat, centers = i, iter.max = 50), silent = TRUE)
        if(inherits(oe, "error")) {
            break
        }
        wss[i] = fit$tot.withinss
        if(is.na(wss[1])) wss[1] = fit$totss
    }
    k = 1:max_km
    k = k[seq_along(wss)]
    if(length(k) == 1) {
        return(1)
    } else if(length(k) == 2) {
        return(2)
    } else {
        min(elbow_finder(k, wss)[1], knee_finder(k, wss)[1])
    }
}

# https://stackoverflow.com/questions/2018178/finding-the-best-trade-off-point-on-a-curve
elbow_finder <- function(x_values, y_values) {
	# Max values to create line
	max_x_x <- max(x_values)
	max_x_y <- y_values[which.max(x_values)]
	max_y_y <- max(y_values)
	max_y_x <- x_values[which.max(y_values)]
	max_df <- data.frame(x = c(max_y_x, max_x_x), y = c(max_y_y, max_x_y))

	# Creating straight line between the max values
	fit <- lm(max_df$y ~ max_df$x)

	# Distance from point to line
	distances <- c()
	for(i in 1:length(x_values)) {
	distances <- c(distances, abs(coef(fit)[2]*x_values[i] - y_values[i] + coef(fit)[1]) / sqrt(coef(fit)[2]^2 + 1^2))
	}

	# Max distance point
	x_max_dist <- x_values[which.max(distances)]
	y_max_dist <- y_values[which.max(distances)]

	return(c(x_max_dist, y_max_dist))
}

# https://raghavan.usc.edu//papers/kneedle-simplex11.pdf
knee_finder = function(x, y) {
    n = length(x)
    a = (y[n] - y[1])/(x[n] - x[1])
    b = y[1] - a*x[1]
    d = a*x - y
    x[which.max(d)]
}


shiny_env$is_in_sub_heatmap = FALSE

# == title
# Test whether it is in sub heatmap
#
# == details
# Normally, it is used in ``cell_fun``/``layer_fun``.
#
is_in_sub_heatmap = function() {
	shiny_env$is_in_sub_heatmap
}


validate_heatmap_id = function(id) {
	if(!is.null(id)) {
		id = gsub("\\W+", "_", id)
		if(!grepl("^[a-zA-Z]", id)) {
			id = paste0("v_", id)
		}
		id
	} else {
		id
	}
}