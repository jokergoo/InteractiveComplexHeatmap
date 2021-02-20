# Shiny app development

##############################################################
# title: A single Shiny app with two interactive heatmap widgets.

set.seed(8)
m1 = matrix(rnorm(100*100), 100)
ht1 = Heatmap(m1)
ht1 = draw(ht1)
m2 = matrix(sample(letters[1:10], 100, replace = TRUE), 10)
ht2 = Heatmap(m2)
ht2 = draw(ht2)

ui = fluidPage(
    h3("The first heatmap"),
    InteractiveComplexHeatmapOutput("heatmap_1", height1 = 300, height2 = 300),
    hr(),
    h3("The second heatmap"),
    InteractiveComplexHeatmapOutput("heatmap_2", height1 = 300, height2 = 300)
)

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht1, "heatmap_1")
    makeInteractiveComplexHeatmap(input, output, session, ht2, "heatmap_2")
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
    InteractiveComplexHeatmapOutput(output_ui = htmlOutput("info")),
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
    makeInteractiveComplexHeatmap(input, output, session, ht,
        click_action = click_action, brush_action = brush_action)
}

shinyApp(ui, server)

####################################################################
# title: Self-define the output. Additional annotations for the selected gene are shown.

library(GetoptLong)
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

brush_action = function(df, output) {
    output[["gene_info"]] = renderUI({
        HTML("")
    })
}

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht,
        click_action = click_action, brush_action = brush_action)
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
    makeInteractiveComplexHeatmap(input, output, session, ht,
        click_action = click_action, brush_action = brush_action)
}

shinyApp(ui, server)




##########################################################################
# title: Visualize a DESeq2 results. The selected genes are highlighted in an associated MA plot.

library(airway)
data(airway)
se <- airway
library(DESeq2)
dds <- DESeqDataSet(se, design = ~ dex)
keep <- rowSums(counts(dds)) >= 10
dds <- dds[keep, ]

dds$dex <- relevel(dds$dex, ref = "untrt")

dds <- DESeq(dds)
res <- results(dds)
res = as.data.frame(res)
res = res[, c("baseMean", "log2FoldChange", "padj")]

m = counts(dds, normalized = TRUE)

l = res$padj < 0.01; l[is.na(l)] = FALSE
m = m[l, ]

library(ComplexHeatmap)
library(circlize)

ht = Heatmap(t(scale(t(m))), name = "z-score",
    top_annotation = HeatmapAnnotation(
        dex = colData(dds)$dex,
        sizeFactor = anno_points(colData(dds)$sizeFactor)
    ),
    show_row_names = FALSE, show_column_names = FALSE, row_km = 2,
    column_title = paste0(sum(l), " significant genes with FDR < 0.01"),
    show_row_dend = FALSE) + 
    Heatmap(log10(res$baseMean[l]+1), show_row_names = FALSE, width = unit(5, "mm"),
        name = "log10(baseMean+1)", show_column_names = FALSE) +
    Heatmap(res$log2FoldChange[l], show_row_names = FALSE, width = unit(5, "mm"),
        name = "log2FoldChange", show_column_names = FALSE,
        col = colorRamp2(c(-2, 0, 2), c("green", "white", "red")))
ht = draw(ht, merge_legend = TRUE)

make_maplot = function(res, highlight = NULL) {
    col = rep("#00000020", nrow(res))
    cex = rep(0.5, nrow(res))
    names(col) = rownames(res)
    names(cex) = rownames(res)
    if(is.null(highlight)) {
        l = res$padj < 0.01; l[is.na(l)] = FALSE
        col[l] = "red"
    } else {
        col[highlight] = "red"
        cex[highlight] = 1
    }
    x = res$baseMean
    y = res$log2FoldChange
    y[y > 2] = 2
    y[y < -2] = -2
    col[col == "red" & y < 0] = "darkgreen"
    par(mar = c(4, 4, 1, 1))

    suppressWarnings(
        plot(x, y, col = col, 
            pch = ifelse(res$log2FoldChange > 2 | res$log2FoldChange < -2, 1, 16), 
            cex = cex, log = "x",
            xlab = "baseMean", ylab = "log2 fold change")
    )
}


library(shiny)
ui = fluidPage(
    div(
        InteractiveComplexHeatmapOutput(layout = "1-(2|3)",
            width1 = 400, height1 = 800, width2 = 300, height2 = 300,
            style = "float: left;"),
        div(
            uiOutput("maplot_ui"),
            uiOutput("res_table_ui"),
            style = "float: left;"
        ),
        div(style = "clear: both;")
    )
)

library(DT)
brush_action = function(df, output) {
    
    row_index = unique(unlist(df$row_index))
    selected = rownames(m)[row_index]
        
    output[["maplot_ui"]] = renderUI({   
        output[["maplot"]] = renderPlot({
            make_maplot(res, selected)
        })

        div(
            h5("MA-plot"),
            div(
                plotOutput("maplot", width = 400), 
                style = "border: 1px solid grey; padding: 4px;"
            )
        )
    })

    output[["res_table_ui"]] = renderUI({
        output[["res_table"]] = renderDT(
            formatRound(datatable(res[selected, ], rownames = TRUE), columns = 1:ncol(res), digits = 3)
        )

        div(
            h5("Result table of the selected genes"),
            div(
                DTOutput("res_table"),
                style = "font-size:80%"
            )
        )
    })
}

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht,
        brush_action = brush_action)
}

shinyApp(ui, server)


#########################################################################################################
# title: Interactive correlation heatmap. Clicking on the cell generates a scatterplot of the two corresponding variables.

data(mtcars)
cor_mat = cor(mtcars)

library(circlize)
col_fun = colorRamp2(c(-1, 0, 1), c("darkgreen", "white", "red"))
ht = Heatmap(cor_mat, name = "Correlation",
    col = col_fun, rect_gp = gpar(type = "none"),
    cell_fun = function(j, i, x, y, w, h, fill) {
        grid.rect(x, y, w, h, gp = gpar(fill = "transparent", col = "grey"))
        grid.circle(x = x, y = y, r = abs(cor_mat[i, j])/2 * min(unit.c(w, h)), 
            gp = gpar(fill = col_fun(cor_mat[i, j]), col = NA))
    },
    show_row_dend = FALSE, show_column_dend = FALSE)


ui = fluidPage(
    InteractiveComplexHeatmapOutput(),
    plotOutput("scatterplot", width = 400, height = 400)
)

click_action = function(df, output) {
    output$scatterplot = renderPlot({
        nm = colnames(mtcars)
        i1 = df$row_index
        i2 = df$column_index

        x = mtcars[, nm[i1]]
        y = mtcars[, nm[i2]]

        plot(x, y, xlab = nm[i1], ylab = nm[i2],
            main = paste0("Correlation = ", sprintf('%.3f', cor(x, y))))
    })
}


server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht,
        click_action = click_action)
}

shinyApp(ui, server)

