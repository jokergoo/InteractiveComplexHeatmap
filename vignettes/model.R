grid.newpage()
grid.rect(gp = gpar(lty = 2))
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
seekViewport("mat_a_heatmap_body_1_2")
ht = ht_list@ht_list[["mat_a"]]
m = ht@matrix

i = 1
j = 2
row_order = ht@row_order_list[[i]]
column_order = ht@column_order_list[[j]]
nr = length(row_order)
nc = length(column_order)
grid.segments(1:nc/nc, rep(0, nc), 1:nc/nc, rep(1, nc), default.units = "npc",
	gp = gpar(col = "#888888", lty = 2))
grid.segments(rep(0, nr), 1:nr/nr, rep(1, nr), 1:nr/nr, default.units = "npc",
	gp = gpar(col = "#888888", lty = 2))
grid.rect(gp = gpar(fill = NA))

grid.points(0.3, 0.8, pch = 16, size = unit(2, "mm"), gp = gpar(col = "blue"))
ComplexHeatmap:::grid.text(gt_render("(a, b)", box_gp = gpar(fill = "white", col = NA)), 
	x = unit(0.3, "npc") + unit(2, "mm"), y = unit(0.8, "npc"),
	just = "left")

grid.points(0, 0, pch = 16, size = unit(2, "mm"), gp = gpar(col = "red"))
ComplexHeatmap:::grid.text(gt_render("(x<sub>1</sub>, y<sub>1</sub>)", box_gp = gpar(fill = "white", col = NA)), 
	x = unit(0, "npc") + unit(2, "mm"), y = unit(0, "npc"),
	just = "left")
grid.points(1, 1, pch = 16, size = unit(2, "mm"), gp = gpar(col = "red"))
ComplexHeatmap:::grid.text(gt_render("(x<sub>2</sub>, y<sub>2</sub>)", box_gp = gpar(fill = "white", col = NA)), 
	x = unit(1, "npc"), y = unit(1, "npc") - unit(2, "mm"),
	just = "top")

ComplexHeatmap:::grid.text(gt_render("n<sub>r</sub> = 8", box_gp = gpar(fill = "white", col = NA)), 
	x = unit(1, "npc") + unit(1, "mm"), y = unit(0.5, "npc"),
	just = "left")

ComplexHeatmap:::grid.text(gt_render("n<sub>c</sub> = 5", box_gp = gpar(fill = "white", col = NA)), 
	x = unit(0.5, "npc"), y = unit(1, "npc") + unit(1, "mm"),
	just = "bottom")
