# Shiny app development

##############################################################
# title: A single Shiny app with two interactive heatmap widgets.

m1 = matrix(rnorm(100*100), 100)
ht1 = Heatmap(m1)
ht1 = draw(ht1)
m2 = matrix(sample(letters[1:10], 100, replace = TRUE), 10)
ht2 = Heatmap(m2)
ht2 = draw(ht2)

ui = fluidPage(
    h3("The first heatmap"),
    InteractiveComplexHeatmapOutput("heatmap_1", height1 = 200, height2 = 200),
    hr(),
    h3("The second heatmap"),
    InteractiveComplexHeatmapOutput("heatmap_2", height1 = 200, height2 = 200)
)

server = function(input, output, session) {
    renderInteractiveComplexHeatmap(input, output, session, ht1, "heatmap_1")
    renderInteractiveComplexHeatmap(input, output, session, ht2, "heatmap_2")
}

shinyApp(ui, server)

####################################################################
# title: Self-define the output. The selected sub-matrix is shown as a text table.

library(GetoptLong)
m = matrix(rnorm(100*100), 100)
rownames(m) = paste0("R", 1:100)
colnames(m) = paste0("C", 1:100)
ht = Heatmap(m, show_row_names = FALSE, show_column_names = FALSE, row_km = 2, column_km = 2)
ht = draw(ht)

ui = fluidPage(
    InteractiveComplexHeatmapOutput(),
    htmlOutput("info")
)

click_action = function(df, output) {
    output[["info"]] = renderUI({
        if(!is.null(df)) {
            HTML(qq("<p style='background-color:#FF8080;color:white;padding:5px;'>You have clicked on heatmap @{df$heatmap}, row @{df$row_index}, column @{df$column_index}</p>"))
        }
    })
}

suppressPackageStartupMessages(library(kableExtra))
brush_action = function(df, output) {
    row_index = unique(unlist(df$row_index))
    column_index = unique(unlist(df$column_index))
    output[["info"]] = renderUI({
        if(!is.null(df)) {
            HTML(kable_styling(kbl(m[row_index, column_index, drop = FALSE], digits = 2, format = "html"), full_width = FALSE, position = "left"))
        }
    })
}

server = function(input, output, session) {
    renderInteractiveComplexHeatmap(input, output, session, ht,
        click_action = click_action, brush_action = brush_action,
        default_click_action = FALSE, default_brush_action = FALSE)
}

shinyApp(ui, server)

####################################################################
# title: Self-define the output. Additional annotations for the selected gene are shown.

library(GetoptLong)
suppressPackageStartupMessages(library(EnrichedHeatmap))
load(system.file("extdata", "chr21_test_data.RData", package = "EnrichedHeatmap"))

gene_id = names(rpkm)[1:100]
gene_id = gsub("\\.\\d+$", "", gene_id)

suppressPackageStartupMessages(library(org.Hs.eg.db))
query = AnnotationDbi::select(org.Hs.eg.db, keys = gene_id, columns = c("SYMBOL", "REFSEQ", "UNIPROT"), keytype = "ENSEMBL")
query = split(query, query[, 1])

n = length(query)
m = matrix(rnorm(n*n), n)
rownames(m) = names(query)

ht = Heatmap(m, show_row_names = FALSE)
ht = draw(ht)

ui = fluidPage(
    InteractiveComplexHeatmapOutput(),
    htmlOutput("gene_info")
)

click_action = function(df, output) {
    output[["gene_info"]] = renderUI({
        if(!is.null(df)) {
            g = rownames(m)[df$row_index]

            to_str = function(x) paste(unique(x), collapse = ", ")
            HTML(qq(
"<pre>
Ensembl: <a href='https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=@{g}' target='_blank'>@{g}</a>
SYMBOL: @{to_str(query[[g]][, 'SYMBOL'])}
REFSEQ: @{to_str(query[[g]][, 'REFSEQ'])}
UNIPROT: @{to_str(query[[g]][, 'UNIPROT'])}
</pre>"
))
        }
    })
}

server = function(input, output, session) {
    renderInteractiveComplexHeatmap(input, output, session, ht,
        click_action = click_action)
}

shinyApp(ui, server)


####################################################################
# title: Visualize Gene Ontology similarities. A list of selected GO IDs as well as their descriptions are shown in the output.

library(GetoptLong)
suppressPackageStartupMessages(library(simplifyEnrichment))

mat = readRDS(system.file("extdata", "random_GO_BP_sim_mat.rds",
     package = "simplifyEnrichment"))
cl = binary_cut(mat)
ht = ht_clusters(mat, cl, word_cloud_grob_param = list(max_width = 80))

suppressPackageStartupMessages(library(GO.db))
get_go_term = function(go_id) {
    suppressMessages(AnnotationDbi::select(GO.db, keys = go_id, columns = "TERM")$TERM)
}

ui = fluidPage(
    InteractiveComplexHeatmapOutput(width1 = 700, height1 = 450),
    htmlOutput("go_info")
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
    renderInteractiveComplexHeatmap(input, output, session, ht,
        click_action = click_action, brush_action = brush_action)
}

shinyApp(ui, server)


