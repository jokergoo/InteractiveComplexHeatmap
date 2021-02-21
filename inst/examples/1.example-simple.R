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


###############################################################
# title: Use last generated heatmap, an example from cola package.

library(cola)
data(golub_cola)

res = golub_cola["ATC:skmeans"]
pdf(NULL)
get_signatures(res, k = 3)
dev.off()
htShiny()

###############################################################
# title: Use last generated heatmap, an app with three interactive heatmaps

library(cola)
data(golub_cola)

res = golub_cola["ATC:skmeans"]

pdf(NULL)
consensus_heatmap(res, k = 3)
dev.off()
ht1 = ComplexHeatmap:::get_last_ht()

pdf(NULL)
membership_heatmap(res, k = 3)
dev.off()
ht2 = ComplexHeatmap:::get_last_ht()

pdf(NULL)
get_signatures(res, k = 3)
dev.off()
ht3 = ComplexHeatmap:::get_last_ht()

ui = mainPanel(
    tabsetPanel(
        tabPanel("Consensus heatmap",  InteractiveComplexHeatmapOutput("heatmap_1")),
        tabPanel("Membership heatmap", InteractiveComplexHeatmapOutput("heatmap_2")),
        tabPanel("Signature heatmap",  InteractiveComplexHeatmapOutput("heatmap_3"))
    )
)

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht1, "heatmap_1")
    makeInteractiveComplexHeatmap(input, output, session, ht2, "heatmap_2")
    makeInteractiveComplexHeatmap(input, output, session, ht3, "heatmap_3")
}

shinyApp(ui, server)

###############################################################
# title: Demonstrate hover, click and dblclick actions.

m = matrix(rnorm(10*10), 10)
ht = Heatmap(m)
ht = draw(ht)

ui = tabsetPanel(
    tabPanel("hover",  InteractiveComplexHeatmapOutput("heatmap_1", action = "hover")),
    tabPanel("click", InteractiveComplexHeatmapOutput("heatmap_2", action = "click")),
    tabPanel("dblclick",  InteractiveComplexHeatmapOutput("heatmap_3", action = "dblclick"))
)


server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht, "heatmap_1")
    makeInteractiveComplexHeatmap(input, output, session, ht, "heatmap_2")
    makeInteractiveComplexHeatmap(input, output, session, ht, "heatmap_3")
}

shinyApp(ui, server)

