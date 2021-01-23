.ENV = new.env()

# == title
# Select an area in the heatmap
#
# == param
# -ht_list A `ComplexHeatmap::HeatmapList-class` object returned by `ComplexHeatmap::draw,Heatmap-method` or `ComplexHeatmap::draw,HeatmapList-method`.
# -mark Whether to mark the selected area as a rectangle.
# -pos1 If the value is ``NULL``, it can be selected by click on the heatmap (of cource, the heatmap should be on
#       the interactive graphic device). If it is set, it must be a `grid::unit` object with length two which
#       corresponds to the x and y position of the point.
# -pos2 Another point as ``pos1``, together with ``pos1`` defines the selected region.
# -verbose Whether to print messages.
# -ht_pos A value returned by `htPositionsOnDevice`.
# -include_annotation Internally used.
# -calibrate Internally used. Mainly works for Rstudio desktop IDE.
#
# == details
# The regions can be selected interactively or manually by setting ``pos1`` and ``pos2``.
#
# == value
# A `S4Vectors::DataFrame` object with row indices and column indices corresponding to the selected region.
#
# == example
# if(dev.interactive()) {
#     m = matrix(rnorm(100), 10)
#     rownames(m) = 1:10
#     colnames(m) = 1:10
#     
#     ht = Heatmap(m)
#     ht = draw(ht)
#     selectArea(ht)
#     
#     set.seed(123)
#     ht = Heatmap(m, row_km = 2, column_km = 2)
#     ht = draw(ht)
#     selectArea(ht)
# }
selectArea = function(ht_list = get_last_ht(), pos1 = NULL, pos2 = NULL, mark = TRUE, verbose = TRUE,
	ht_pos = NULL, include_annotation = FALSE, calibrate = TRUE) {

	if(missing(ht_list)) {
		stop_wrap("The heatmap object must be provided.")
	}

	if(calibrate) {
		if(validate_RStudio_desktop()) {
			message_wrap("Automatically regenerate previous heatmap.")
			draw(ht_list)
		}
	}

	if(is.null(pos1) || is.null(pos2)) pos2 = pos1 = NULL
	if(!is.null(pos1)) {
		if(getRversion() >= "4.0.0") {
			if(!inherits(pos1, "simpleUnit")) {
				stop_wrap("`pos1` should be a simpleUnit.")
			}
			if(!inherits(pos2, "simpleUnit")) {
				stop_wrap("`pos2` should be a simpleUnit.")
			}
		} else {
			if(!inherits(pos1, "unit")) {
				stop_wrap("`pos1` should be a simple unit.")
			}
			if(!inherits(pos2, "unit")) {
				stop_wrap("`pos2` should be a simple unit.")
			}
		}

		if(length(pos1) != 2) {
			stop_wrap("Length of `pos1` should be 2 (x and y).")
		}

		if(length(pos2) != 2) {
			stop_wrap("Length of `pos2` should be 2 (x and y).")
		}

		if(!identical(unitType(pos1), unitType(pos2))) {
			stop_wrap("`pos1` should have the same unit as `pos2`.")
		}

		pos1 = list(x = pos1[1], y = pos1[2])
		pos2 = list(x = pos2[1], y = pos2[2])
	}

	if(mark) {
		oe = try(seekViewport("global"), silent = TRUE)
		if(inherits(oe, "try-error")) {
			stop_wrap("Cannot find the global viewport. You need to draw the heatmap or go to the device which contains the heatmap.")
		}
		upViewport()
	}
	
	if(is.null(pos1)) {
		if(!dev.interactive()) {
			stop_wrap("Graphic device should be interactive if you want to manually select the regions.")
		}

		if(verbose) cat("Click two positions on the heatmap (double click or right click on\nthe plot to cancel):\n")

		unit = "mm"
		pos1 = grid.locator(unit = unit)
		if(is.null(pos1)) {
			if(verbose) cat("Canceled.\n")
			return(invisible(NULL))
		}
	} else {
		unit = unitType(pos1$x)
	}
	pos1 = lapply(pos1, as.numeric)
	if(mark) grid.points(pos1$x, pos1$y, default.units = unit)
	if(verbose) qqcat("  Point 1: x = @{sprintf('%.1f', pos1$x)} @{unit}, y = @{sprintf('%.1f', pos1$y)} @{unit} (measured in the graphics device)\n")
	
	if(is.null(pos2)) {
		unit = "mm"
		pos2 = grid.locator(unit = unit)
		if(is.null(pos2)) {
			if(verbose) cat("Canceled.\n")
			return(invisible(NULL))
		}
	} else {
		unit = unitType(pos2$x)
	}
	pos2 = lapply(pos2, as.numeric)
	if(mark) grid.points(pos2$x, pos2$y, default.units = unit)
	if(verbose) qqcat("  Point 2: x = @{sprintf('%.1f', pos2$x)} @{unit}, y = @{sprintf('%.1f', pos2$y)} @{unit} (measured in the graphics device)\n")
	
	if(verbose) cat("\n")

	if(mark) {
		grid.rect( (0.5*pos1$x + 0.5*pos2$x), (0.5*pos1$y + 0.5*pos2$y),
			       pos2$x - pos1$x, pos2$y - pos1$y,
			       default.units = unit, gp = gpar(fill = NA) )
	}

	#### pos1 should always be on the bottom left and pos2 on the top right
	if(pos1$x > pos2$x) {
		tmp = pos1$x
		pos1$x = pos2$x
		pos2$x = tmp
	}
	if(pos1$y > pos2$y) {
		tmp = pos1$y
		pos1$y = pos2$y
		pos2$y = tmp
	}

	if(is.null(ht_pos)) {
		if(is.null(.ENV$previous_ht_hash)) {
			ht_pos = htPositionsOnDevice(ht_list, include_annotation = include_annotation)
		} else {
			if(!identical(.ENV$previous_device_size, dev.size())) {
				if(verbose) {
					cat("The device size has been changed. Calcualte the new heatmap positions.\n")
				}
				ht_pos = htPositionsOnDevice(ht_list, include_annotation = include_annotation)
			} else if(!identical(.ENV$previous_ht_hash, digest(list(ht_list, include_annotation)))) {
				if(verbose) {
					cat("The heatmaps have been changed. Calcualte the new heatmap positions.\n")
				}
				ht_pos = htPositionsOnDevice(ht_list, include_annotation = include_annotation)
			} else {
				if(verbose) {
					cat("Heatmap positions are already calculated, use the cached one.\n")
				}
				ht_pos = .ENV$previous_ht_pos_on_device
			}
		}
	}
	ht_pos$x_min = convertX(ht_pos$x_min, unit, valueOnly = TRUE)
	ht_pos$x_max = convertX(ht_pos$x_max, unit, valueOnly = TRUE)
	ht_pos$y_min = convertX(ht_pos$y_min, unit, valueOnly = TRUE)
	ht_pos$y_max = convertX(ht_pos$y_max, unit, valueOnly = TRUE)

	df = NULL
	overlap_to_heatmap = FALSE
	for(i in seq_len(nrow(ht_pos))) {

		ht_name = ht_pos[i, "heatmap"]

		ht = ht_list@ht_list[[ht_name]]

		vp_min_x = ht_pos[i, "x_min"]
		vp_max_x = ht_pos[i, "x_max"]
		vp_min_y = ht_pos[i, "y_min"]
		vp_max_y = ht_pos[i, "y_max"]

		if(inherits(ht, "Heatmap")) {

			if(verbose) qqcat("Search in heatmap '@{ht_name}'\n")

			slice_name = ht_pos[i, "slice"]
			i_slice = as.numeric(gsub(".*_(\\d+)_\\d+$", "\\1", slice_name))
			j_slice = as.numeric(gsub(".*_(\\d+)$", "\\1", slice_name))

			if(verbose) qqcat("  - row slice @{i_slice}, column slice @{j_slice} [@{slice_name}]... ")
			
			row_index = integer(0)
			column_index = integer(0)

			nc = length(ht@column_order_list[[j_slice]])
			ind1 = ceiling((pos1$x - vp_min_x) / (vp_max_x - vp_min_x) * nc)
			ind2 = ceiling((pos2$x - vp_min_x) / (vp_max_x - vp_min_x) * nc)
			if(ind1 <= 0 && ind2 <= 0) { # the region is on the left of the heatmap
				column_index = integer(0)
			} else if(ind1 > nc && ind2 > nc) { # the region in on the right of the heatmap
				column_index = integer(0)
			} else {
				if(ind1 <= 0) ind1 = 1
				if(ind2 >= nc) ind2 = nc

				if(ind1 < ind2) {
					column_index = ht@column_order_list[[j_slice]][ind1:ind2]
				} else {
					column_index = ht@column_order_list[[j_slice]][ind2:ind1]
				}
			}

			nr = length(ht@row_order_list[[i_slice]])
			ind1 = 1 + nr - ceiling((pos1$y - vp_min_y) / (vp_max_y - vp_min_y) * nr)
			ind2 = 1 + nr - ceiling((pos2$y - vp_min_y) / (vp_max_y - vp_min_y) * nr)
			if(ind1 <= 0 && ind2 <= 0) { # the region is on the bottom of the heatmap
				row_index = integer(0)
			} else if(ind1 > nr && ind2 > nr) { # the region in on the top of the heatmap
				row_index = integer(0)
			} else {
				if(ind2 <= 0) ind2 = 1
				if(ind1 >= nr) ind1 = nr
				if(ind1 < ind2) {
					row_index = ht@row_order_list[[i_slice]][ind1:ind2]
				} else {
					row_index = ht@row_order_list[[i_slice]][ind2:ind1]
				}
			}

			if(length(column_index) == 0) row_index = integer(0)
			if(length(row_index) == 0) column_index = integer(0)
			
			if(length(column_index) == 0) {
				if(verbose) cat("no overlap\n")
			} else {
				if(verbose) cat("overlap\n")

				overlap_to_heatmap = TRUE
				df = rbind(df, S4Vectors::DataFrame(heatmap = ht_name, 
						           slice = slice_name, 
						           row_slice = i_slice,
					               column_slice = j_slice,
						           row_index = IntegerList(row_index), 
						           column_index = IntegerList(column_index)))
			}
		} else {
			if(include_annotation) {

				if(verbose) qqcat("Search in heatmap annotation '@{ht_name}'\n")

				if(ht_list@direction == "horizontal") {
					if( (pos1$x <= vp_min_x && pos2$x >= vp_min_x) ||
						(pos1$x <= vp_max_x && pos2$x >= vp_max_x) ||
						(pos1$x >= vp_min_x && pos2$x <= vp_min_x) ||
						(pos1$x <= vp_min_x && pos2$x >= vp_max_x) ) {
						df = rbind(df, S4Vectors::DataFrame(heatmap = ht_name, 
							           slice = NA, 
							           row_slice = NA,
						               column_slice = NA,
							           row_index = IntegerList(0), 
							           column_index = IntegerList(0)))
					}
				} else {
					if( (pos1$y <= vp_min_y && pos2$y >= vp_min_y) ||
						(pos1$y <= vp_max_y && pos2$y >= vp_max_y) ||
						(pos1$y >= vp_min_y && pos2$y <= vp_min_y) ||
						(pos1$y <= vp_min_y && pos2$y >= vp_max_y) ) {
						df = rbind(df, S4Vectors::DataFrame(heatmap = ht_name, 
							           slice = NA, 
							           row_slice = NA,
						               column_slice = NA,
							           row_index = IntegerList(0), 
							           column_index = IntegerList(0)))
					}
				}
			}
		}
	
	}
	if(verbose) cat("\n")

	if(mark) {
		for(i in seq_len(nrow(ht_pos))) {
		    x_min = ht_pos[i, "x_min"]
		    x_max = ht_pos[i, "x_max"]
		    y_min = ht_pos[i, "y_min"]
		    y_max = ht_pos[i, "y_max"]
		    grid.rect(x = x_min, y = y_min,
		        width = x_max - x_min, height = y_max - y_min, gp = gpar(fill = "transparent"),
		        just = c("left", "bottom"))
		}
	}

	if(overlap_to_heatmap) {
		return(df)
	} else {
		if(verbose) cat("The selected area does not overlap to any heatmap.\n")
		return(NULL)
	} 
}

# == title
# Select a position in the heatmap
#
# == param
# -ht_list A `ComplexHeatmap::HeatmapList-class` object returned by `ComplexHeatmap::draw,Heatmap-method` or `ComplexHeatmap::draw,HeatmapList-method`.
# -mark Whether to mark the selected position as a point.
# -pos If the value is ``NULL``, it can be selected by click on the heatmap (of cource, the heatmap should be on
#       the interactive graphic device). If it is set, it must be a `grid::unit` object with length two which
#       corresponds to the x and y position of the point.
# -verbose Whether to print messages.
# -ht_pos A value returned by `htPositionsOnDevice`.
# -calibrate Internally used.
#
# == value
# A `S4Vectors::DataFrame` object with row indices and column indices corresponding to the selected position.
#
# == example
# if(dev.interactive()) {
#     m = matrix(rnorm(100), 10)
#     rownames(m) = 1:10
#     colnames(m) = 1:10
#     
#     ht = Heatmap(m)
#     ht = draw(ht)
#     selectPosition(ht)
# }
selectPosition = function(ht_list = get_last_ht(), pos = NULL, mark = TRUE, verbose = TRUE,
	ht_pos = NULL, calibrate = TRUE) {

	if(missing(ht_list)) {
		stop_wrap("The heatmap object must be provided.")
	}

	if(calibrate) {
		if(validate_RStudio_desktop()) {
			message_wrap("Automatically regenerate previous heatmap.")
			draw(ht_list)
		}
	}

	pos1 = pos
	if(!is.null(pos1)) {
		if(getRversion() >= "4.0.0") {
			if(!inherits(pos1, "simpleUnit")) {
				stop_wrap("`pos` should be a simpleUnit.")
			}
		} else {
			if(!inherits(pos1, "unit")) {
				stop_wrap("`pos` should be a simple unit.")
			}
		}

		if(length(pos1) != 2) {
			stop_wrap("Length of `pos` should be 2 (x and y).")
		}

		pos1 = list(x = pos1[1], y = pos1[2])
	}

	if(mark) {
		oe = try(seekViewport("global"), silent = TRUE)
		if(inherits(oe, "try-error")) {
			stop_wrap("Cannot find the global viewport. You need to draw the heatmap or go to the device which contains the heatmap.")
		}
		upViewport()
	}

	if(is.null(pos1)) {
		if(!dev.interactive()) {
			stop_wrap("Graphic device should be interactive if you want to manually select the position.")
		}

		if(verbose) cat("Click one position on the heatmap (right click on the plot to cancel):\n")

		unit = "mm"
		pos1 = grid.locator(unit = unit)
		if(is.null(pos1)) {
			if(verbose) cat("Canceled.\n")
			return(invisible(NULL))
		}
	} else {
		unit = unitType(pos1$x)
	}
	pos1 = lapply(pos1, as.numeric)
	if(mark) grid.points(pos1$x, pos1$y, default.units = unit)
	if(verbose) qqcat("  Point: x = @{sprintf('%.1f', pos1$x)} @{unit}, y = @{sprintf('%.1f', pos1$y)} @{unit} (measured in the graphics device)\n")
	
	if(verbose) cat("\n")

	if(is.null(ht_pos)) {
		if(is.null(.ENV$previous_ht_hash)) {
			ht_pos = htPositionsOnDevice(ht_list)
		} else {
			if(!identical(.ENV$previous_device_size, dev.size())) {
				if(verbose) {
					cat("The device size has been changed. Calculate new heatmap positions.\n")
				}
				ht_pos = htPositionsOnDevice(ht_list)
			} else if(!identical(.ENV$previous_ht_hash, digest(list(ht_list, TRUE))) || 
				      !identical(.ENV$previous_ht_hash, digest(list(ht_list, FALSE)))) {
				if(verbose) {
					cat("The heatmaps have been changed. Calculate new heatmap positions.\n")
				}
				ht_pos = htPositionsOnDevice(ht_list)
			} else {
				if(verbose) {
					cat("Heatmap positions are already calculated, use the cached one.\n")
				}
				ht_pos = .ENV$previous_ht_pos_on_device
			}
		}
	}

	# seekViewport("global")
	# upViewport()
	# if(mark) {
	# 	for(i in seq_len(nrow(ht_pos))) {
	# 	    x_min = ht_pos[i, "x_min"]
	# 	    x_max = ht_pos[i, "x_max"]
	# 	    y_min = ht_pos[i, "y_min"]
	# 	    y_max = ht_pos[i, "y_max"]
	# 	    grid.rect(x = x_min, y = y_min,
	# 	        width = x_max - x_min, height = y_max - y_min, gp = gpar(fill = "transparent"),
	# 	        just = c("left", "bottom"))
	# 	}
	# }

	ht_pos$x_min = convertX(ht_pos$x_min, unit, valueOnly = TRUE)
	ht_pos$x_max = convertX(ht_pos$x_max, unit, valueOnly = TRUE)
	ht_pos$y_min = convertX(ht_pos$y_min, unit, valueOnly = TRUE)
	ht_pos$y_max = convertX(ht_pos$y_max, unit, valueOnly = TRUE)

	df = NULL
	for(i in seq_len(nrow(ht_pos))) {

		ht_name = ht_pos[i, "heatmap"]

		ht = ht_list@ht_list[[ht_name]]

		if(!inherits(ht, "Heatmap")) next

		if(verbose) qqcat("Search in heatmap '@{ht_name}'\n")

		slice_name = ht_pos[i, "slice"]
		i_slice = as.numeric(gsub(".*_(\\d+)_\\d+$", "\\1", slice_name))
		j_slice = as.numeric(gsub(".*_(\\d+)$", "\\1", slice_name))

		if(verbose) qqcat("  - row slice @{i_slice}, column slice @{j_slice} [@{slice_name}]... ")
		
		vp_min_x = ht_pos[i, "x_min"]
		vp_max_x = ht_pos[i, "x_max"]
		vp_min_y = ht_pos[i, "y_min"]
		vp_max_y = ht_pos[i, "y_max"]

		row_index = integer(0)
		column_index = integer(0)

		nc = length(ht@column_order_list[[j_slice]])
		ind1 = ceiling((pos1$x - vp_min_x) / (vp_max_x - vp_min_x) * nc)

		if(ind1 <= 0 || ind1 > nc) { # the region is on the left of the heatmap
			column_index = integer(0)
		} else {
			column_index = ht@column_order_list[[j_slice]][ind1]
		}

		nr = length(ht@row_order_list[[i_slice]])
		ind1 = 1 + nr - ceiling((pos1$y - vp_min_y) / (vp_max_y - vp_min_y) * nr)
		if(ind1 <= 0  || ind1 > nr) { # the region is on the bottom of the heatmap
			row_index = integer(0)
		} else {
			row_index = ht@row_order_list[[i_slice]][ind1]
		}

		if(length(column_index) == 0) row_index = integer(0)
		if(length(row_index) == 0) column_index = integer(0)
		
		if(length(column_index) == 0) {
			if(verbose) cat("no overlap\n")
		} else {
			if(verbose) cat("overlap\n")

			df = S4Vectors::DataFrame(heatmap = ht_name, 
				           slice = slice_name, 
				           row_slice = i_slice,
				           column_slice = j_slice,
				           row_index = row_index, 
				           column_index = column_index)
			return(df)
		}

	}
	if(verbose) cat("\n")
	if(verbose) cat("The selected position does not sit in any heatmap.\n")
	return(NULL)
}


# == title
# Get the heatmap positions on the graphic device
#
# == param
# -ht_list A `ComplexHeatmap::HeatmapList-class` object returned by `ComplexHeatmap::draw,Heatmap-method` or `ComplexHeatmap::draw,HeatmapList-method`.
# -unit The unit.
# -valueOnly Whether only return the numeric values.
# -include_annotation Internally used.
# -calibrate Internally used.
#
# == value
# It returns a `S4Vectors::DataFrame` object of the position of every heatmap slice.
#
# == example
# if(dev.interactive()) {
#     m = matrix(rnorm(100), 10)
#     ht = Heatmap(m, row_km = 2, column_km = 2)
#     ht = draw(ht)
#     pos = htPositionsOnDevice(ht)
#     
#     InteractiveComplexHeatmap:::redraw_ht_vp(pos)
# }
htPositionsOnDevice = function(ht_list = get_last_ht(), unit = "inch", valueOnly = FALSE, 
	include_annotation = FALSE, calibrate = TRUE) {
	
	if(calibrate) {
		if(validate_RStudio_desktop()) {
			message_wrap("Automatically regenerate previous heatmap.")
			draw(ht_list)
		}
	}

	if(!is.null(.ENV$RStudio_png_res)) {
		ds = dev.size("px")
		dev.copy(png, file = tempfile(), width = ds[1], height = ds[2], res = .ENV$RStudio_png_res)
	}
	
	oe = try(seekViewport("global"), silent = TRUE)
	if(inherits(oe, "try-error")) {
		stop_wrap("No heatmap is on the graphic device.")
	}
	upViewport() # to ROOT

	if(inherits(ht_list, "Heatmap")) {
		stop_wrap("`ht_list` should be returned by `draw()`.")
	}

	if(!ht_list@layout$initialized) {
		stop_wrap("`ht_list` should be returned by `draw()`.")
	}

	all_ht_names = names(ht_list@ht_list)
	all_ht_names = all_ht_names[sapply(ht_list@ht_list, inherits, "Heatmap")]
	l = duplicated(all_ht_names)
	if(any(l)) {
		stop_wrap("Heatmap names should not be duplicated.")
	}

	
	df = NULL
	has_normal_matrix = FALSE
	ht_main = ht_list@ht_list[[ ht_list@ht_list_param$main_heatmap ]]
	for(i in seq_along(ht_list@ht_list)) {
		if(inherits(ht_list@ht_list[[i]], "Heatmap")) {
			ht = ht_list@ht_list[[i]]
			ht_name = ht@name

			if(nrow(ht@matrix) == 0 || ncol(ht@matrix) == 0) {
				next
			}

			has_normal_matrix = TRUE

			for(i in seq_along(ht@row_order_list)) {
				for(j in seq_along(ht@column_order_list)) {

					vp_name = qq("@{ht_name}_heatmap_body_@{i}_@{j}")

					seekViewport(vp_name)
					loc = deviceLoc(x = unit(0, "npc"), y = unit(0, "npc"))
					vp_min_x = loc[[1]]
					vp_min_y = loc[[2]]

					loc = deviceLoc(x = unit(1, "npc"), y = unit(1, "npc"))
					vp_max_x = loc[[1]]
					vp_max_y = loc[[2]]
					
					df = rbind(df, S4Vectors::DataFrame(heatmap = ht_name,
						                     slice = vp_name,
						                     row_slice = i,
						                     column_slice = j,
						                     x_min = as.numeric(vp_min_x),
						                     x_max = as.numeric(vp_max_x),
						                     y_min = as.numeric(vp_min_y),
						                     y_max = as.numeric(vp_max_y)))
					
				}
			}
		} else {
			if(include_annotation) {
				if(ht_list@direction == "horizontal") {
					ht = ht_list@ht_list[[i]]
					ht_name = ht@name

					j = 1
					i = 1

					vp_name = qq("heatmap_@{ht_name}")

					seekViewport(vp_name)
					loc = deviceLoc(x = unit(0, "npc"), y = unit(0, "npc"))
					vp_min_x = convertX(loc[[1]], unit)
					vp_min_y = convertY(loc[[2]], unit)

					loc = deviceLoc(x = unit(1, "npc"), y = unit(1, "npc"))
					vp_max_x = convertX(loc[[1]], unit)
					vp_max_y = convertY(loc[[2]], unit)
					
					df = rbind(df, S4Vectors::DataFrame(heatmap = ht_name,
						                     slice = vp_name,
						                     row_slice = NA,
						                     column_slice = NA,
						                     x_min = as.numeric(vp_min_x),
						                     x_max = as.numeric(vp_max_x),
						                     y_min = NA,
							                 y_max = NA))
					
				} else {
					ht = ht_list@ht_list[[i]]
					ht_name = ht@name

					i = 1
					j = 1

					vp_name = qq("heatmap_@{ht_name}")

					seekViewport(vp_name)
					loc = deviceLoc(x = unit(0, "npc"), y = unit(0, "npc"))
					vp_min_x = convertX(loc[[1]], unit)
					vp_min_y = convertY(loc[[2]], unit)

					loc = deviceLoc(x = unit(1, "npc"), y = unit(1, "npc"))
					vp_max_x = convertX(loc[[1]], unit)
					vp_max_y = convertY(loc[[2]], unit)
					
					df = rbind(df, S4Vectors::DataFrame(heatmap = ht_name,
						                     slice = vp_name,
						                     row_slice = NA,
						                     column_slice = NA,
						                     x_min = NA,
						                     x_max = NA,
						                     y_min = as.numeric(vp_min_y),
						                     y_max = as.numeric(vp_max_y)))
					
				}
			}
		}
	}

	if(!is.null(.ENV$RStudio_png_res)) { 
		dev.off()
	}

	if(!has_normal_matrix) {
		stop_wrap("There should be one normal heatmap (nrow > 0 and ncol > 0) in the heatmap list.")
	}

	if(!valueOnly) {
		df$x_min = unit(df$x_min, unit)
		df$x_max = unit(df$x_max, unit)
		df$y_min = unit(df$y_min, unit)
		df$y_max = unit(df$y_max, unit)

		.ENV$previous_ht_pos_on_device = df
		.ENV$previous_ht_hash = digest(list(ht_list, include_annotation))
		.ENV$previous_device_size = dev.size()
	}

	seekViewport("global")
	upViewport()

	df
}

redraw_ht_vp = function(pos) {
	ds = dev.size()
	dev.new(width = ds[1], height = ds[2])
	grid.newpage()
	
	for(i in seq_len(nrow(pos))) {
		x_min = pos[i, "x_min"]
		x_max = pos[i, "x_max"]
		y_min = pos[i, "y_min"]
		y_max = pos[i, "y_max"]
		pushViewport(viewport(x = x_min, y = y_min, name = pos[i, "slice"],
			width = x_max - x_min, height = y_max - y_min,
			just = c("left", "bottom")))
		grid.rect()
		upViewport()
	}
}

seek_root_vp = function() {
	seekViewport(grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)$name[2])
	upViewport(1)
}

# given a vector of row keywords or column keywords, 
selectByLabels = function(ht_list = get_last_ht(), row_keywords = NULL, column_keywords = NULL, 
	keyword_is_regexpr = FALSE, heatmap = NULL, 
	all = TRUE, include_annotation = FALSE) {

	if(inherits(ht_list, "Heatmap")) {
		stop_wrap("`ht_list` should be returned by `draw()`.")
	}

	if(!ht_list@layout$initialized) {
		stop_wrap("`ht_list` should be returned by `draw()`.")
	}

	df = NULL
	if(!is.null(row_keywords) && !is.null(column_keywords)) {
		if(length(heatmap) > 1) {
			stop_wrap("If `row_keywords` and `column_keywords` are both set, `heatmap` should only be NULL or only contain one heatmap.")
		}
		if(length(heatmap) == 0) {
			which_heatmap = which(sapply(ht_list@ht_list, function(x) inherits(x, "Heatmap")))
			if(length(which_heatmap) != 1) {
				stop_wrap("If `row_keywords` and `column_keywords` are both set, and `heatmap` is NULL, `ht_list` should only contain one heatmap.")
			}

			heatmap = names(ht_list@ht_list)[which_heatmap]
		}

		ht_name = heatmap
		ht = ht_list@ht_list[[heatmap]]
		row_labels = ht@row_names_param$label
		column_labels = ht@column_names_param$label
		if(!is.null(row_labels)) {
			for(k in seq_along(row_keywords)) {
				if(keyword_is_regexpr) {
					ri = grep(row_keywords[k], row_labels)
				} else {
					ri = which(row_labels %in% row_keywords[k])
				}
				if(length(ri)) {
					for(i_slice in seq_along(ht@row_order_list)) {
						if(any(ht@row_order_list[[i_slice]] %in% ri)) {

							if(!is.null(column_labels)) {
								for(k in seq_along(column_keywords)) {
									if(keyword_is_regexpr) {
										ci = grep(column_keywords[k], column_labels)
									} else {
										ci = which(column_labels %in% column_keywords[k])
									}
									if(length(ci)) {
										for(j_slice in seq_along(ht@column_order_list)) {
											if(any(ht@column_order_list[[j_slice]] %in% ci)) {
												for(i_slice in seq_along(ht@row_order_list)) {
														df = rbind(df, S4Vectors::DataFrame(heatmap = ht_name, 
																				           slice = qq("@{ht_name}_heatmap_body_@{i_slice}_@{j_slice}"), 
																				           row_slice = i_slice,
																			               column_slice = j_slice,
																				           row_index = IntegerList(intersect(ht@row_order_list[[i_slice]], ri)), 
																				           column_index = IntegerList(intersect(ht@column_order_list[[j_slice]], ci))))
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}	
		if(is.null(df)) return(df)
	} else {

		for(i in seq_along(ht_list@ht_list)) {
			ht_name = ht_list@ht_list[[i]]@name
			ht = ht_list@ht_list[[ht_name]]
			if(inherits(ht, "Heatmap")) {
				if(!is.null(heatmap)) {
					if(!ht_name %in% heatmap) next
				}
				row_labels = ht@row_names_param$label
				column_labels = ht@column_names_param$label
				if(!is.null(row_keywords)) {
					if(!is.null(row_labels)) {
						for(k in seq_along(row_keywords)) {
							if(keyword_is_regexpr) {
								ri = grep(row_keywords[k], row_labels)
							} else {
								ri = which(row_labels %in% row_keywords[k])
							}
							if(length(ri)) {
								for(i_slice in seq_along(ht@row_order_list)) {
									if(any(ht@row_order_list[[i_slice]] %in% ri)) {
										for(j_slice in seq_along(ht@column_order_list)) {
											df = rbind(df, S4Vectors::DataFrame(heatmap = ht_name, 
																	           slice = qq("@{ht_name}_heatmap_body_@{i_slice}_@{j_slice}"), 
																	           row_slice = i_slice,
																               column_slice = j_slice,
																	           row_index = IntegerList(intersect(ht@row_order_list[[i_slice]], ri)), 
																	           column_index = IntegerList(ht@column_order_list[[j_slice]])))
										}
									}
								}
							}
						}
					}
				} else if(!is.null(column_keywords)) {
					if(!is.null(column_labels)) {
						for(k in seq_along(column_keywords)) {
							if(keyword_is_regexpr) {
								ci = grep(column_keywords[k], column_labels)
							} else {
								ci = which(column_labels %in% column_keywords[k])
							}
							if(length(ci)) {
								for(j_slice in seq_along(ht@column_order_list)) {
									if(any(ht@column_order_list[[j_slice]] %in% ci)) {
										for(i_slice in seq_along(ht@row_order_list)) {
											df = rbind(df, S4Vectors::DataFrame(heatmap = ht_name, 
																	           slice = qq("@{ht_name}_heatmap_body_@{i_slice}_@{j_slice}"), 
																	           row_slice = i_slice,
																               column_slice = j_slice,
																	           row_index = IntegerList(ht@row_order_list[[j_slice]]), 
																	           column_index = IntegerList(intersect(ht@column_order_list[[j_slice]], ci))))
										}
									}
								}
							}
						}
					}
				}
			}
		}

		if(is.null(df)) return(df)

		ndf = nrow(df)
		if(all && !is.null(df)) {
			for(i in seq_along(ht_list@ht_list)) {
				ht_name = ht_list@ht_list[[i]]@name
				ht = ht_list@ht_list[[ht_name]]
				if(inherits(ht, "Heatmap")) {
					for(idf in seq_len(ndf)) {
						if(!ht_name %in% df$heatmap[idf]) {
							if(ht_list@direction == "horizontal") {
								if(!is.null(row_keywords)) {
									for(j_slice in seq_along(ht@column_order_list)) {
										i_slice = df$row_slice[idf]
										df = rbind(df, S4Vectors::DataFrame(heatmap = ht_name, 
																           slice = qq("@{ht_name}_heatmap_body_@{i_slice}_@{j_slice}"), 
																           row_slice = i_slice,
															               column_slice = j_slice,
																           row_index = df$row_index[idf], 
																           column_index = IntegerList(ht@column_order_list[[j_slice]])))
									}
								}
							} else {
								if(!is.null(column_keywords)) {
									for(i_slice in seq_along(ht@row_order_list)) {
										j_slice = df$column_slice[idf]
										df = rbind(df, S4Vectors::DataFrame(heatmap = ht_name, 
																           slice = qq("@{ht_name}_heatmap_body_@{i_slice}_@{j_slice}"), 
																           row_slice = i_slice,
															               column_slice = j_slice,
																           row_index = IntegerList(ht@row_order_list[[j_slice]]), 
																           column_index = df$column_index[idf]))
									}
								}
							}
						}
					}
				} else {
					if(include_annotation) {
						df = rbind(df, S4Vectors::DataFrame(heatmap = ht_name, 
												           slice = NA, 
												           row_slice = NA,
											               column_slice = NA,
												           row_index = IntegerList(0), 
												           column_index = IntegerList(0)))
					}
				}
			}
		}
	}
	reformat_df(df, ht_list)
}

reformat_df = function(df, ht_list) {
	
	if(is.null(df)) return(df)

	df_ht = df[!is.na(df$slice), , drop = FALSE]
	df_anno = df[is.na(df$slice), , drop = FALSE]
	
	fa = paste(df_ht[, 1], df_ht[, 2], df_ht[, 3], df_ht[, 4], sep = ":")
	df_ht = S4Vectors::DataFrame(heatmap = as.vector(tapply(df_ht$heatmap, fa, function(x) x[1])), 
			           slice = as.vector(tapply(df_ht$slice, fa, function(x) x[1])), 
			           row_slice = as.vector(tapply(df_ht$row_slice, fa, function(x) x[1])),
		               column_slice = as.vector(tapply(df_ht$column_slice, fa, function(x) x[1])),
			           row_index = IntegerList(tapply(df_ht$row_index, fa, function(lt) unique(unlist(lt)))), 
			           column_index = IntegerList(tapply(df_ht$column_index, fa, function(lt) unique(unlist(lt))))
	)

	for(i in seq_len(nrow(df_ht))) {
		df_ht$row_index[[i]] = intersect(ht_list@ht_list[[ df_ht$heatmap[i] ]]@row_order_list[[ df_ht$row_slice[i] ]], df_ht$row_index[[i]])
		df_ht$column_index[[i]] = intersect(ht_list@ht_list[[ df_ht$heatmap[i] ]]@column_order_list[[ df_ht$column_slice[i] ]], df_ht$column_index[[i]])
	}
	rbind(df_ht, df_anno)
}
