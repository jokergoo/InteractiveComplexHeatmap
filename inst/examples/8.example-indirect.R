# Interactivate indirect use of pheatmap(), heatmap.2() and heatmap()

########################################################
# title: Indirect use of pheatmap().

assignInNamespace("pheatmap", ComplexHeatmap::pheatmap, ns = "pheatmap")

library(SingleCellExperiment)
library(SC3)
library(scater)

sce <- SingleCellExperiment(
    assays = list(
        counts = as.matrix(yan),
        logcounts = log2(as.matrix(yan) + 1)
    ), 
    colData = ann
)

rowData(sce)$feature_symbol <- rownames(sce)
sce <- sce[!duplicated(rowData(sce)$feature_symbol), ]
sce <- runPCA(sce)
sce <- sc3(sce, ks = 2:4, biology = TRUE)

sc3_plot_expression(sce, k = 3)
htShiny()

##########################################################
# title: Indirect use of heatmap.2().

require(gplots)
assignInNamespace("heatmap.2", ComplexHeatmap:::heatmap.2, ns = "gplots")

library(GOexpress)
data(AlvMac)
set.seed(4543)
AlvMac_results <- GO_analyse(
	eSet = AlvMac, f = "Treatment",
	GO_genes=AlvMac_GOgenes, all_GO=AlvMac_allGO, all_genes=AlvMac_allgenes)
BP.5 <- subset_scores(
	result = AlvMac_results.pVal,
	namespace = "biological_process",
	total = 5,
	p.val=0.05)
heatmap_GO(
	go_id = "GO:0034142", result = BP.5, eSet=AlvMac, cexRow=0.4,
	cexCol=1, cex.main=1, main.Lsplit=30)

heatmap_GO(
	go_id = "GO:0034142", result = BP.5, eSet=AlvMac, cexRow=0.4,
	cexCol=1, cex.main=1, main.Lsplit=30)
htShiny()


########################################################
# title: Two interactive heatmap widgets from indirect use of pheatmap().

assignInNamespace("pheatmap", ComplexHeatmap::pheatmap, ns = "pheatmap")

p1 = function(mat) {
	pheatmap::pheatmap(mat, col = c("white", "red"))
}

p2= function(mat) {
	pheatmap::pheatmap(mat, col = c("white", "blue"))
}

mat = matrix(rnorm(100), 10)

pdf(NULL)
draw(p1(mat))
dev.off()
ht1 = ComplexHeatmap:::get_last_ht()

pdf(NULL)
draw(p2(mat))
dev.off()
ht2 = ComplexHeatmap:::get_last_ht()

ui = mainPanel(
    InteractiveComplexHeatmapOutput("heatmap_1"),
    InteractiveComplexHeatmapOutput("heatmap_2")
)

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht1, "heatmap_1")
    makeInteractiveComplexHeatmap(input, output, session, ht2, "heatmap_2")
}

shinyApp(ui, server)

