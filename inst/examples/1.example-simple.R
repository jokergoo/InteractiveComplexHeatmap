# Simple examples

####################################################################
# title: A single heatmap with minimal arguments.

m = matrix(rnorm(100*100), 100)
ht = Heatmap(m)
ht = draw(ht)

htShiny(ht)

####################################################################
# title: A single heatmap from a character matrix.

m = matrix(sample(letters[1:10], 100*100, replace = TRUE), 100)
ht = Heatmap(m)
ht = draw(ht)

htShiny(ht)

####################################################################
# title: A single heatmap with annotations on both rows and columns.

m = matrix(rnorm(100*100), 100)
rownames(m) = paste0("R", 1:100)
colnames(m) = paste0("C", 1:100)
ht = Heatmap(m, 
	top_annotation = HeatmapAnnotation(foo = runif(100)),
	left_annotation = rowAnnotation(bar = anno_points(1:100)),
	show_row_names = FALSE, show_column_names = FALSE)
ht = draw(ht)

htShiny(ht)

####################################################################
# title: A single heatmap where rows and columns are split.

m = matrix(rnorm(100*100), 100)
rownames(m) = paste0("R", 1:100)
colnames(m) = paste0("C", 1:100)
ht = Heatmap(m, 
	top_annotation = HeatmapAnnotation(foo = runif(100)),
	left_annotation = rowAnnotation(bar = anno_points(1:100)),
	show_row_names = FALSE, show_column_names = FALSE,
	row_km = 2, column_km = 3)
ht = draw(ht)

htShiny(ht)

####################################################################
# title: A list of two heatmaps.

m = matrix(rnorm(100*100), 100)
rownames(m) = paste0("R", 1:100)
colnames(m) = paste0("C", 1:100)
ht1 = Heatmap(m, 
	top_annotation = HeatmapAnnotation(foo1 = runif(100)),
	left_annotation = rowAnnotation(bar1 = anno_points(1:100)),
	show_row_names = FALSE, show_column_names = FALSE)

m = matrix(rnorm(100*100), 100)
rownames(m) = paste0("R", 1:100)
colnames(m) = paste0("C", 1:100)
ht2 = Heatmap(m, 
	bottom_annotation = HeatmapAnnotation(foo2 = runif(100)),
	right_annotation = rowAnnotation(bar2 = anno_points(1:100)),
	show_row_names = FALSE, show_column_names = FALSE)

ht_list = ht1 + ht2
ht_list = draw(ht_list)
htShiny(ht_list, width1 = 600)
