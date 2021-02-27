# Float output UI along with mouse positions

##########################################################
# title: A simple example that demonstrates output UI floating with the three actions: hover, click and dblclick.

set.seed(123)
mat1 = matrix(rnorm(100), 10)
rownames(mat1) = colnames(mat1) = paste0("a", 1:10)
mat2 = matrix(sample(letters[1:10], 100, replace = TRUE), 10)
rownames(mat2) = colnames(mat2) = paste0("b", 1:10)
ht_list = Heatmap(mat1, name = "mat_a", row_km = 2, column_km = 2,
        top_annotation = HeatmapAnnotation(foo = anno_points(runif(10)))) +
    rowAnnotation(bar = anno_barplot(sample(10, 10))) +
    Heatmap(mat2, name = "mat_b")

ht_list = draw(ht_list)

ui = fluidPage(
    tabsetPanel(
    	tabPanel("action = 'click'", InteractiveComplexHeatmapOutput("ht1", output_ui_float = TRUE, action = "click")),
    	tabPanel("action = 'hover'", InteractiveComplexHeatmapOutput("ht2", output_ui_float = TRUE, action = "hover")),
    	tabPanel("action = 'dblclick'", InteractiveComplexHeatmapOutput("ht3", output_ui_float = TRUE, action = "dblclick"))
   	)
)

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht_list, "ht1")
    makeInteractiveComplexHeatmap(input, output, session, ht_list, "ht2")
    makeInteractiveComplexHeatmap(input, output, session, ht_list, "ht3")
}

shinyApp(ui, server)

########################################################
# title: floating self-defined outputs.

library(GetoptLong)
suppressPackageStartupMessages(library(simplifyEnrichment))

mat = readRDS(system.file("extdata", "random_GO_BP_sim_mat.rds",
     package = "simplifyEnrichment"))
cl = binary_cut(mat)
ht = ht_clusters(mat, cl, word_cloud_grob_param = list(max_width = 80))

suppressPackageStartupMessages(library(GO.db))
get_go_term = function(go_id) {
    term = suppressMessages(AnnotationDbi::select(GO.db, keys = go_id, columns = "TERM")$TERM)
    term[is.na(term)] = "NA"
    term
}

ui = fluidPage(
	p("After brushing from heatmap, keep the mouse position unchanged. After about 1 second, you will see the output for the brush."),
    InteractiveComplexHeatmapOutput(width1 = 700, height1 = 450, output_ui_float = TRUE,
    	output_ui = htmlOutput("go_info"))
)

library(GetoptLong)
click_action = function(df, output) {
    output[["go_info"]] = renderUI({
        if(!is.null(df)) {
            go_id1 = rownames(mat)[df$row_index]
            go_id2 = colnames(mat)[df$column_index]

            HTML(qq(
"<pre>
## Row GO ID
<a href='http://amigo.geneontology.org/amigo/term/@{go_id1}' target='_blank'>@{go_id1}</a>: @{get_go_term(go_id1)}

## Column GO ID:
<a href='http://amigo.geneontology.org/amigo/term/@{go_id2}' target='_blank'>@{go_id2}</a>: @{get_go_term(go_id2)}
</pre>"
))
        }
    })
}

brush_action = function(df, output) {
    output[["go_info"]] = renderUI({
        if(!is.null(df)) {
            row_index = unique(unlist(df$row_index))
            column_index = unique(unlist(df$column_index))
            go_id1 = rownames(mat)[row_index]
            go_id2 = colnames(mat)[column_index]

            go_id = union(go_id1, go_id2)

            go_text = qq("<a href='http://amigo.geneontology.org/amigo/term/@{go_id}' target='_blank'>@{go_id}</a>: @{get_go_term(go_id)}\n")
            HTML(qq(
"<pre>
@{go_text}
</pre>"
))
        }
    })
}

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht,
        click_action = click_action, brush_action = brush_action)
}

shinyApp(ui, server)

