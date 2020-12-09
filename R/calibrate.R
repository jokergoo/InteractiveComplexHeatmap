
calibrate = function() {
	grid.newpage()
	pushViewport(viewport())
	grid.points(unit(0.5, "npc"), 0.4)
	grid.points(convertX(unit(0.5, "npc"), "in"), 0.6)

	loc1 = grid.locator("in"); grid.points(loc1$x, loc1$y, pch = 3, gp = gpar(col = 2))
	loc2 = grid.locator("in"); grid.points(loc2$x, loc2$y, pch = 3, gp = gpar(col = 2))

	loc1 = sapply(loc1, as.numeric)
	loc2 = sapply(loc2, as.numeric)

	if( abs(loc2["x"] - loc1["x"]) > 0.01) {
		.ENV$RStudio_png_res = 96
	}
}

validate_RStudio_desktop = function() {
	if(is_RStudio_current_dev() && interactive()) {
		if(exists("RStudio.Version")) {
			if(get("RStudio.Version")()$mode == "desktop") {
				if(is.null(.ENV$RStudio_png_res)) {
					message(paste(strwrap("Detect you are using RStudio desktop. To use the interactive ComplexHeatmap, you need to make some calibrations on the graphics device (the right bottom figure panel). You current heatmap will be lost and you might need to generate it again after the calibration. This calibration only happens once. [y/n]"), collapse = "\n"), appendLF = FALSE)

					answer = readline()
			        if(!(answer == "y" || answer == "Y")) {
			        	stop_wrap("Calibration is refused.")
			        }

					message_wrap("In the following plot, use your mouse to click on the two points.\n")
					calibrate()

					if(!is.null(.ENV$RStudio_png_res)) {
						message_wrap("Done! Set the internal PNG resolution to 96. Now you can regenerate your heatmap and use the interactive functionality from ComplexHeatmap.\n")
					} else {
						message_wrap("Done! No need for calibration. Now you can regenerate your heatmap and use the interactive functionality from ComplexHeatmap.\n")
					}
					return(TRUE)
				}
			}
		}
	}
	return(FALSE)
}
