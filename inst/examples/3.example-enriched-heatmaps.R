# Enriched heatmaps

####################################################################
# title: An enriched heatmap.

suppressPackageStartupMessages(library(EnrichedHeatmap))
load(system.file("extdata", "chr21_test_data.RData", package = "EnrichedHeatmap"))

tss = promoters(genes, upstream = 0, downstream = 1)
mat1 = normalizeToMatrix(H3K4me3, tss, value_column = "coverage", 
    extend = 5000, mean_mode = "w0", w = 50)

suppressPackageStartupMessages(library(circlize))
col_fun = colorRamp2(quantile(mat1, c(0, 0.99)), c("white", "red"))

ht = EnrichedHeatmap(mat1, col = col_fun, name = "H3K4me3", row_km = 3,
    column_title = "Enrichment of H3K4me3", row_title_rot = 0)
ht = draw(ht)

htShiny(ht, width1 = 300, height1 = 600)

####################################################################
# title: A list of enriched heatmaps.

suppressPackageStartupMessages(library(EnrichedHeatmap))
load(system.file("extdata", "chr21_test_data.RData", package = "EnrichedHeatmap"))

tss = promoters(genes, upstream = 0, downstream = 1)
mat1 = normalizeToMatrix(H3K4me3, tss, value_column = "coverage", 
    extend = 5000, mean_mode = "w0", w = 50)

suppressPackageStartupMessages(library(circlize))
col_fun = colorRamp2(quantile(mat1, c(0, 0.99)), c("white", "red"))

mat2 = normalizeToMatrix(meth, tss, value_column = "meth", mean_mode = "absolute",
    extend = 5000, w = 50, background = NA, smooth = TRUE)
meth_col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))

ht_list = EnrichedHeatmap(mat1, col = col_fun, name = "H3K4me3",
	    top_annotation = HeatmapAnnotation(enrich = anno_enriched(axis_param = list(side = "left")))) + 
	EnrichedHeatmap(mat2, col = meth_col_fun, name = "methylation") +
	Heatmap(log2(rpkm+1), col = c("white", "orange"), name = "log2(rpkm+1)", 
	    show_row_names = FALSE, width = unit(5, "mm"))
ht_list = draw(ht_list)

htShiny(ht_list, width1 = 600, height1 = 600)

####################################################################
# title: An enriched heatmap with discrete signals.

suppressPackageStartupMessages(library(EnrichedHeatmap))

if(0) {
# note the following code takes several minutes to run, so the result objects are already generated
# and you will find the links after the end of this code chunk. (This code chunk is runnable.)
suppressPackageStartupMessages(library(GenomicRanges))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(circlize))

states_bed = fread("http://egg2.wustl.edu/roadmap/data/byFileType/chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final/E003_15_coreMarks_mnemonics.bed.gz")
states = GRanges(seqnames = states_bed[[1]], 
    ranges = IRanges(states_bed[[2]] + 1, states_bed[[3]]), 
    states = states_bed[[4]])

map = c(
    "1_TssA"      = "TssActive",
    "2_TssAFlnk"  = "TssActive",
    "3_TxFlnk"    = "Transcript",
    "4_Tx"        = "Transcript",
    "5_TxWk"      = "Transcript",
    "6_EnhG"      = "Enhancer",
    "7_Enh"       = "Enhancer",
    "8_ZNF/Rpts"  = "Heterochromatin",
    "9_Het"       = "Heterochromatin",
    "10_TssBiv"   = "TssBivalent",
    "11_BivFlnk"  = "TssBivalent",
    "12_EnhBiv"   = "Enhancer",
    "13_ReprPC"   = "Repressive",
    "14_ReprPCWk" = "Repressive",
    "15_Quies"    = "Quiescent"
)
states$states_simplified = map[states$states]

states_col = c(
    "TssActive"       = "Red",
    "Transcript"      = "Green",
    "Enhancer"        = "Yellow",
    "Heterochromatin" = "PaleTurquoise",
    "TssBivalent"     = "Orange",
    "Repressive"      = "Grey",
    "Quiescent"       = "black"
)
states_name = names(states_col)
n_states = length(states_col)

suppressPackageStartupMessages(library(GenomicFeatures))
# following code build the txdb object
download.file("http://egg2.wustl.edu/roadmap/data/byDataType/rna/annotations/gen10.long.gtf.gz", "gen10.long.gtf.gz")
txdb = makeTxDbFromGFF("gen10.long.gtf.gz")  # this takes long runtime
file.remove("gen10.long.gtf.gz")

g = genes(txdb)
tss = promoters(g, upstream = 0, downstream = 1)
tss_chr1 = tss[seqnames(tss) == "chr1"]
# column "states_simplified" is in character mode
mat_states = normalizeToMatrix(states, tss_chr1, value_column = "states_simplified")

saveRDS(mat_states, file = "~/project/jokergoo.github.io/public/files/interactive_complexheatmap_mat_states.rds")
saveRDS(states_col, file = "~/project/jokergoo.github.io/public/files/interactive_complexheatmap_states_col.rds")

}

mat_states = readRDS(url("https://jokergoo.github.io/files/interactive_complexheatmap_mat_states.rds"))
states_col = readRDS(url("https://jokergoo.github.io/files/interactive_complexheatmap_states_col.rds"))


ht = EnrichedHeatmap(mat_states, name = "states", col = states_col, cluster_rows = TRUE)
ht = draw(ht)

htShiny(ht, width1 = 300, height1 = 600)
