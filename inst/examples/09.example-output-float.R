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

# Note the output from brusing also floats.
ui = fluidPage(
    tabsetPanel(
    	tabPanel("action = 'click'",    InteractiveComplexHeatmapOutput("ht1", output_ui_float = TRUE, action = "click")),
    	tabPanel("action = 'hover'",    InteractiveComplexHeatmapOutput("ht2", output_ui_float = TRUE, action = "hover")),
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
# title: Floating self-defined outputs.

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
    InteractiveComplexHeatmapOutput(width1 = 700, height1 = 450, output_ui_float = TRUE,
    	output_ui = htmlOutput("go_info"))
)

# The following click_action and brush_action are self-defined actions corresponding to the click
# and brush events on heatmap.
library(GetoptLong)
click_action = function(df, output) {
    output[["go_info"]] = renderUI({
        if(!is.null(df)) {
            go_id1 = rownames(mat)[df$row_index]
            go_id2 = colnames(mat)[df$column_index]

            oe = try(term1 <- get_go_term(go_id1), silent = TRUE)
            if(inherits(oe, "try-error")) {
                term1 = ""
            }
            oe = try(term2 <- get_go_term(go_id2), silent = TRUE)
            if(inherits(oe, "try-error")) {
                term2 = ""
            }

            v = mat[go_id1, go_id2]
            col = col_fun(v)

            HTML(qq(
"<div style='padding:5px 10px;border:1px solid black; width:600px; background-color:white;'>
<h5>GO similarity</h5>
<p>@{sprintf('%.3f', v)}  <span style='background-color:@{col};width=10px;'>&nbsp;&nbsp;&nbsp;&nbsp;</span></p>
<h5>Row GO ID</h5>
<p><a href='http://amigo.geneontology.org/amigo/term/@{go_id1}' target='_blank'>@{go_id1}</a>: @{term1}</p>
<h5>Column GO ID</h5>
<p><a href='http://amigo.geneontology.org/amigo/term/@{go_id2}' target='_blank'>@{go_id2}</a>: @{term2}</p>
</div>"
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

            go_text = qq("<p><a href='http://amigo.geneontology.org/amigo/term/@{go_id}' target='_blank'>@{go_id}</a>: @{get_go_term(go_id)}</p>\n")
            HTML(qq(
"<div style='padding:5px 10px;border:1px solid black; width:600px; background-color:white;'>
@{go_text}
</div>"
))
        }
    })
}

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht,
        click_action = click_action, brush_action = brush_action)
}

shinyApp(ui, server)


########################################################################
# title: Floating output only from one event on heatmap, i.e. hover/click/dblclick/brush-output.

m = matrix(rnorm(10*10), 10)
ht = Heatmap(m)
ht = draw(ht)

ui = tabsetPanel(
    tabPanel("response = 'hover'",  InteractiveComplexHeatmapOutput("heatmap_1", action = "hover", 
        response = "hover", output_ui_float = TRUE)),
    tabPanel("response = 'click'", InteractiveComplexHeatmapOutput("heatmap_2", action = "click", 
        response = "click", output_ui_float = TRUE)),
    tabPanel("response = 'dblclick'", InteractiveComplexHeatmapOutput("heatmap_3", action = "dblclick", 
        response = "dblclick", output_ui_float = TRUE)),
    tabPanel("response = 'brush-output'",  InteractiveComplexHeatmapOutput("heatmap_4", 
        response = "brush-output", output_ui_float = TRUE))
)

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht, "heatmap_1")
    makeInteractiveComplexHeatmap(input, output, session, ht, "heatmap_2")
    makeInteractiveComplexHeatmap(input, output, session, ht, "heatmap_3")
    makeInteractiveComplexHeatmap(input, output, session, ht, "heatmap_4")
}

shinyApp(ui, server)


