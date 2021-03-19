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

# Remember now each `InteractiveComplexHeatmapOutput()` must specify the heatmap ID.
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

# We directly assign the new UI to the argument `output_ui` to replace the default output UI.
ui = fluidPage(
    InteractiveComplexHeatmapOutput(output_ui = htmlOutput("info")),
)

# We define one action for click event and one action for brush event on heatmap.
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
            HTML(kable_styling(kbl(m[row_index, column_index, drop = FALSE], digits = 2, format = "html"), 
                full_width = FALSE, position = "left"))
        }
    })
}

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht,
        click_action = click_action, brush_action = brush_action)
}

shinyApp(ui, server)

####################################################################
# title: Self-define the output. Additional annotations for the selected genes are shown.

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

# Here the new output is independent to the default output UI.
ui = fluidPage(
    InteractiveComplexHeatmapOutput(),
    htmlOutput("gene_info")
)

# We add more annotations for the selected genes.
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

# This new brush action basicaly removes the output created by click_action
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
    term = suppressMessages(AnnotationDbi::select(GO.db, keys = go_id, columns = "TERM")$TERM)
    term[is.na(term)] = "NA"
    term
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
    InteractiveComplexHeatmapOutput(response = "click", 
        output_ui = plotOutput("scatterplot", width = 400, height = 400))
)

click_action = function(df, output) {
    output$scatterplot = renderPlot({
        if(is.null(df)) {
            grid.text("You should click on heatmap cells.")
        } else {
            nm = colnames(mtcars)
            i1 = df$column_index
            i2 = df$row_index

            x = mtcars[, nm[i1]]
            y = mtcars[, nm[i2]]

            plot(x, y, xlab = nm[i1], ylab = nm[i2],
                main = paste0("Correlation = ", sprintf('%.3f', cor(x, y))))
        }
    })
}


server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht,
        click_action = click_action)
}

shinyApp(ui, server)


#############################################################################################
# title: A heatmap on Jaccard coefficients for a list of genomic regions. Clicking on the cell generates a Hilbert curve of how the two sets of genomic regions overlap.

library(GenomicRanges)
library(HilbertCurve)
library(ComplexHeatmap)
library(GetoptLong)

file_list = c(
    "IDH_DMV" = "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE121nnn/GSE121721/suppl/GSE121721_IDH_methylation_features_DMV.bed.gz",
    "IDH_LMR" = "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE121nnn/GSE121721/suppl/GSE121721_IDH_methylation_features_LMR.bed.gz",
    "IDH_PMD" = "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE121nnn/GSE121721/suppl/GSE121721_IDH_methylation_features_PMD.bed.gz",

    "MES_DMV" = "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE121nnn/GSE121721/suppl/GSE121721_MES_methylation_features_DMV.bed.gz",
    "MES_LMR" = "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE121nnn/GSE121721/suppl/GSE121721_MES_methylation_features_LMR.bed.gz",
    "MES_PMD" = "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE121nnn/GSE121721/suppl/GSE121721_MES_methylation_features_PMD.bed.gz",

    "RTK_I_DMV" = "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE121nnn/GSE121721/suppl/GSE121721_RTK_I_methylation_features_DMV.bed.gz",
    "RTK_I_LMR" = "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE121nnn/GSE121721/suppl/GSE121721_RTK_I_methylation_features_LMR.bed.gz",
    "RTK_I_PMD" = "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE121nnn/GSE121721/suppl/GSE121721_RTK_I_methylation_features_PMD.bed.gz",

    "RTK_II_DMV" = "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE121nnn/GSE121721/suppl/GSE121721_RTK_II_methylation_features_DMV.bed.gz",
    "RTK_II_LMR" = "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE121nnn/GSE121721/suppl/GSE121721_RTK_II_methylation_features_LMR.bed.gz",
    "RTK_II_PMD" = "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE121nnn/GSE121721/suppl/GSE121721_RTK_II_methylation_features_PMD.bed.gz"
)

gr_list = lapply(file_list, function(x) {
    tmp = tempfile()
    download.file(x, tmp)
    df = read.table(tmp)
    file.remove(tmp)
    GRanges(seqnames = df[, 1], ranges = IRanges(df[, 2], df[, 3]))
})

# to get the function `genomic_regions_correlation()` that calculates Jaccard coeffcients.
source("https://raw.githubusercontent.com/jokergoo/epik/master/R/genomic_region_correlation.R")

jaccard_mat = genomic_regions_correlation(gr_list, gr_list)$stat

ht = draw(Heatmap(jaccard_mat, col = c("white", "red"), name = "Jaccard"))

make_hilbert_curve = function(i, j) {

    gr1 = gr_list[[i]]
    gr2 = gr_list[[j]]

    name1 = names(gr_list)[i]
    name2 = names(gr_list)[j]

    gr1_unique = setdiff(gr1, gr2)
    gr_common = intersect(gr1, gr2)
    gr2_unique = setdiff(gr2, gr1)

    gr = c(gr1_unique, gr_common, gr2_unique)

    l = as.vector(seqnames(gr) == "chr21")

    lgd = Legend(at = c(paste0("unique in ", name1), "in both", paste0("unique in ", name2)), legend_gp = gpar(fill = c("green", "red", "blue")))
    hc = GenomicHilbertCurve(chr = "chr21", level = 6, reference = TRUE, arrow = FALSE,
        legend = lgd, title = qq("Compare @{name1} and @{name2}, jaccard = @{sprintf('%.3f', jaccard_mat[i, j])}"))
    hc_segments(hc, gr, 
        gp = gpar(col = c(rep("green", length(gr1_unique)),
                         rep("red", length(gr_common)),
                         rep("blue", length(gr2_unique))),
                  lwd = 6))
}

ui = fluidPage(
    InteractiveComplexHeatmapOutput(response = "click", 
        width1 = 600, height1 = 600, 
        output_ui = plotOutput("hcplot", width = 700, height = 600))
    
)

click_action = function(df, output) {
    output$hcplot = renderPlot({
        if(is.null(df)) {
            grid.text("You should click on heatmap cells.")
        } else {
            i1 = df$column_index
            i2 = df$row_index
            make_hilbert_curve(i1, i2)
        }
    })
}

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht,
        click_action = click_action)
}

shinyApp(ui, server)
