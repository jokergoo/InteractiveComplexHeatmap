# title: A single heatmap with minimal arguments.

m = matrix(rnorm(100*100), 100)
ht = Heatmap(m)

ht_shiny(ht)

# title: A single heatmap from a character matrix.

m = matrix(sample(letters[1:10], 100*100, replace = TRUE), 100)
ht = Heatmap(m)

ht_shiny(ht)

# title: With heatmap annotations on both rows and columns.

m = matrix(rnorm(100*100), 100)
rownames(m) = paste0("R", 1:100)
colnames(m) = paste0("C", 1:100)
ht = Heatmap(m, 
	top_annotation = HeatmapAnnotation(foo = runif(100)),
	left_annotation = rowAnnotation(bar = anno_points(1:100)),
	show_row_names = FALSE, show_column_names = FALSE)

ht_shiny(ht)

# title: A single heatmap where rows and columns are split.

m = matrix(rnorm(100*100), 100)
rownames(m) = paste0("R", 1:100)
colnames(m) = paste0("C", 1:100)
ht = Heatmap(m, 
	top_annotation = HeatmapAnnotation(foo = runif(100)),
	left_annotation = rowAnnotation(bar = anno_points(1:100)),
	show_row_names = FALSE, show_column_names = FALSE,
	row_km = 2, column_km = 3)

ht_shiny(ht)

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

ht_shiny(ht1 + ht2, width1 = 600)

# title: A density heatmap.

m = matrix(rnorm(100*100), 100)
ht = densityHeatmap(m)

ht_shiny(ht)

# title: An oncoPrint.

mat = read.table(system.file("extdata", package = "ComplexHeatmap", 
	"tcga_lung_adenocarcinoma_provisional_ras_raf_mek_jnk_signalling.txt"), 
	header = TRUE, stringsAsFactors = FALSE, sep = "\t")
mat[is.na(mat)] = ""
rownames(mat) = mat[, 1]
mat = mat[, -1]
mat=  mat[, -ncol(mat)]
mat = t(as.matrix(mat))

col = c("HOMDEL" = "blue", "AMP" = "red", "MUT" = "#008000")
alter_fun = list(
	background = function(x, y, w, h) {
		grid.rect(x, y, w-unit(2, "pt"), h-unit(2, "pt"), 
			gp = gpar(fill = "#CCCCCC", col = NA))
	},
	# big blue
	HOMDEL = function(x, y, w, h) {
		grid.rect(x, y, w-unit(2, "pt"), h-unit(2, "pt"), 
			gp = gpar(fill = col["HOMDEL"], col = NA))
	},
	# big red
	AMP = function(x, y, w, h) {
		grid.rect(x, y, w-unit(2, "pt"), h-unit(2, "pt"), 
			gp = gpar(fill = col["AMP"], col = NA))
	},
	# small green
	MUT = function(x, y, w, h) {
		grid.rect(x, y, w-unit(2, "pt"), h*0.33, 
			gp = gpar(fill = col["MUT"], col = NA))
	}
)

column_title = "OncoPrint for TCGA Lung Adenocarcinoma, genes in Ras Raf MEK JNK signalling"
heatmap_legend_param = list(title = "Alternations", at = c("HOMDEL", "AMP", "MUT"), 
		labels = c("Deep deletion", "Amplification", "Mutation"))
ht = oncoPrint(mat,
	alter_fun = alter_fun, col = col, 
	remove_empty_columns = TRUE, remove_empty_rows = TRUE,
	top_annotation = HeatmapAnnotation(cbar = anno_oncoprint_barplot(),
		foo1 = 1:172,
		bar1 = anno_points(1:172)
	),
	left_annotation = rowAnnotation(foo2 = 1:26),
	right_annotation = rowAnnotation(bar2 = anno_barplot(1:26)),
	column_title = column_title, heatmap_legend_param = heatmap_legend_param)

ht_shiny(ht, width1 = 800)


# title: A UpSet plot.

movies = read.csv(system.file("extdata", "movies.csv", package = "UpSetR"), 
    header = TRUE, sep = ";")
m = make_comb_mat(movies, top_n_sets = 10)
m = m[comb_degree(m) > 0]
ht = UpSet(m)

ht_shiny(ht, width1 = 800)

# title: An interactive heatmap by `pheatmap()`.

test = matrix(rnorm(200), 20, 10)
test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3
test[11:20, seq(2, 10, 2)] = test[11:20, seq(2, 10, 2)] + 2
test[15:20, seq(2, 10, 2)] = test[15:20, seq(2, 10, 2)] + 4
colnames(test) = paste("Test", 1:10, sep = "")
rownames(test) = paste("Gene", 1:20, sep = "")

annotation_col = data.frame(
    CellType = factor(rep(c("CT1", "CT2"), 5)), 
    Time = 1:5
)
rownames(annotation_col) = paste("Test", 1:10, sep = "")

annotation_row = data.frame(
    GeneClass = factor(rep(c("Path1", "Path2", "Path3"), c(10, 4, 6)))
)
rownames(annotation_row) = paste("Gene", 1:20, sep = "")

ann_colors = list(
    Time = c("white", "firebrick"),
    CellType = c(CT1 = "#1B9E77", CT2 = "#D95F02"),
    GeneClass = c(Path1 = "#7570B3", Path2 = "#E7298A", Path3 = "#66A61E")
)

p = pheatmap(test, annotation_col = annotation_col, annotation_row = annotation_row, 
    annotation_colors = ann_colors)

ht_shiny(p)

# title: An interactive heatmap by `heatmap()`.

x  = as.matrix(mtcars)
rc = rainbow(nrow(x), start = 0, end = 0.3)
cc = rainbow(ncol(x), start = 0, end = 0.3)

## note `heatmap()` should be from ComplexHeatmap package
ht = ComplexHeatmap::heatmap(x, col = cm.colors(256), scale = "column",
              RowSideColors = rc, ColSideColors = cc, margins = c(5,10),
              xlab = "specification variables", ylab =  "Car Models",
              main = "heatmap(<Mtcars data>, ..., scale = \"column\")")
ht_shiny(ht)


# title: An interactive heatmap by `heatmap.2()`.

data(mtcars)
x = as.matrix(mtcars)

# note `heatmap.2()` should be from ComplexHeatmap package
ht = ComplexHeatmap::heatmap.2(x, col = gplots::bluered, scale = "column", tracecol = "#303030")

ht_shiny(ht)

# title: An enriched heatmap.

library(EnrichedHeatmap)
load(system.file("extdata", "chr21_test_data.RData", package = "EnrichedHeatmap"))

tss = promoters(genes, upstream = 0, downstream = 1)
mat1 = normalizeToMatrix(H3K4me3, tss, value_column = "coverage", 
    extend = 5000, mean_mode = "w0", w = 50)

library(circlize)
col_fun = colorRamp2(quantile(mat1, c(0, 0.99)), c("white", "red"))

ht = EnrichedHeatmap(mat1, col = col_fun, name = "H3K4me3", row_km = 3,
    column_title = "Enrichment of H3K4me3", row_title_rot = 0)

ht_shiny(ht, width1 = 300, height1 = 600)

# title: A list of multiple enriched heatmaps.

library(EnrichedHeatmap)
load(system.file("extdata", "chr21_test_data.RData", package = "EnrichedHeatmap"))

tss = promoters(genes, upstream = 0, downstream = 1)
mat1 = normalizeToMatrix(H3K4me3, tss, value_column = "coverage", 
    extend = 5000, mean_mode = "w0", w = 50)

library(circlize)
col_fun = colorRamp2(quantile(mat1, c(0, 0.99)), c("white", "red"))

mat2 = normalizeToMatrix(meth, tss, value_column = "meth", mean_mode = "absolute",
    extend = 5000, w = 50, background = NA, smooth = TRUE)
meth_col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))

ht = EnrichedHeatmap(mat1, col = col_fun, name = "H3K4me3",
    top_annotation = HeatmapAnnotation(enrich = anno_enriched(axis_param = list(side = "left")))) + 
EnrichedHeatmap(mat2, col = meth_col_fun, name = "methylation") +
Heatmap(log2(rpkm+1), col = c("white", "orange"), name = "log2(rpkm+1)", 
    show_row_names = FALSE, width = unit(5, "mm"))

ht_shiny(ht, width1 = 600, height1 = 600)

# title: An enriched heatmap with discrete signals. Note, this example may take long time for preparing the data for heatmaps.

library(GenomicRanges)
library(data.table)
library(EnrichedHeatmap)
library(circlize)

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

library(GenomicFeatures)
# following code build the txdb object
download.file("http://egg2.wustl.edu/roadmap/data/byDataType/rna/annotations/gen10.long.gtf.gz", "gen10.long.gtf.gz")
txdb = makeTxDbFromGFF("gen10.long.gtf.gz")  # this takes long runtime
file.remove("gen10.long.gtf.gz")

g = genes(txdb)
tss = promoters(g, upstream = 0, downstream = 1)
tss_chr1 = tss[seqnames(tss) == "chr1"]
# column "states_simplified" is in character mode
mat_states = normalizeToMatrix(states, tss_chr1, value_column = "states_simplified")

ht = EnrichedHeatmap(mat_states, name = "states", col = states_col, cluster_rows = TRUE)

ht_shiny(ht, width1 = 300, height1 = 600)

# title: A heatmap produced from tidyHeatmap package.

library(tidyverse)
library(tidyHeatmap)
mtcars_tidy <- 
    mtcars %>% 
    as_tibble(rownames="Car name") %>% 
    
    # Scale
    mutate_at(vars(-`Car name`, -hp, -vs), scale) %>%
    
    # tidyfy
    pivot_longer(cols = -c(`Car name`, hp, vs), names_to = "Property", values_to = "Value")

mtcars_heatmap <- 
    mtcars_tidy %>% 
        heatmap(`Car name`, Property, Value ) %>%
        add_tile(hp)

ht_shiny(mtcars_heatmap)

# title: An example from Lewis et al 2019. GitHub repo: https://github.com/kevinblighe/E-MTAB-6141

require(RColorBrewer)
require(ComplexHeatmap)
require(circlize)
require(digest)
require(cluster)

mat <- read.table('https://github.com/kevinblighe/E-MTAB-6141/raw/master/rdata/mat.tsv', sep = '\t', row.names = 1,
	header = TRUE, stringsAsFactors = FALSE)

metadata <- read.table('https://github.com/kevinblighe/E-MTAB-6141/raw/master/rdata/metadata.tsv', sep = '\t', row.names = 1,
	header = TRUE, stringsAsFactors = FALSE)

sig_genes <- read.table('https://github.com/kevinblighe/E-MTAB-6141/raw/master/rdata/sig_genes.list', sep = '\t',
	header = FALSE, stringsAsFactors = FALSE)[,1]

mat <- mat[sig_genes,]

heat <- t(scale(t(mat)))
myCol <- colorRampPalette(c('dodgerblue', 'black', 'yellow'))(100)
myBreaks <- seq(-3, 3, length.out = 100)

# CD3
cd3 <- metadata$CD3
cd3 <- cd3[!is.na(cd3)] # remove missing values - we don't want to include these in the mapping
pick.col <- brewer.pal(9, 'Greens')
col.cd3 <- colorRampPalette(pick.col)(length(unique(cd3)))

# CD20
cd20 <- metadata$CD20
cd20 <- cd20[!is.na(cd20)]
pick.col <- brewer.pal(9, 'Blues')
col.cd20 <- colorRampPalette(pick.col)(length(unique(cd20)))

# CD68L
cd68L <- metadata$CD68L
cd68L <- cd68L[!is.na(cd68L)]
pick.col <- brewer.pal(9, 'Reds')
col.cd68L <- colorRampPalette(pick.col)(length(unique(cd68L)))

# CD68SL
cd68SL <- metadata$CD68SL
cd68SL <- cd68L[!is.na(cd68L)]
pick.col <- brewer.pal(9, 'Oranges')
col.cd68SL <- colorRampPalette(pick.col)(length(unique(cd68SL)))

# CD138
cd138 <- metadata$CD138
cd138 <- cd138[!is.na(cd138)]
pick.col <- brewer.pal(9, 'Purples')
col.cd138 <- colorRampPalette(pick.col)(length(unique(cd68SL)))

# Create an initial data-frame of the annotation that we want to use
# In this example, the 'ann' object turns out to be the exact same as 'metadata'
ann <- data.frame(
	Pathotype = metadata$Pathotype,
	CD3 = metadata$CD3,
	CD20 = metadata$CD20,
	CD68L = metadata$CD68L,
	CD68SL = metadata$CD68SL,
	CD138 = metadata$CD138,
	stringsAsFactors = FALSE
)

# create the colour mapping
colours <- list(
	Pathotype = c('Lymphoid' = 'blue', 'Myeloid' = 'red', 'Fibroid' = 'green3', 'Ungraded' = 'grey'),
	CD3 = c('0' = '#F7FCF5', '1' = '#C7E9C0', '2' = '#74C476', '3' = '#238B45', '4' = '#00441B'),
	CD20 = c('0' = '#F7FBFF', '1' = '#C6DBEF', '2' = '#6BAED6', '3' = '#2171B5', '4' = '#08306B'),
	CD68L = c('0' = '#FFF5F0', '1' = '#FCBBA1', '2' = '#FB6A4A', '3' = '#CB181D', '4' = '#67000D'),
	CD68SL = c('0' = '#FFF5EB', '1' = '#FDD0A2', '2' = '#FD8D3C', '3' = '#D94801', '4' = '#7F2704'),
	CD138 = c('0' = '#FCFBFD', '1' = '#DADAEB', '2' = '#9E9AC8', '3' = '#6A51A3', '4' = '#3F007D')
)

# now create the ComplexHeatmap annotation object
# as most of these parameters are self-explanatory, comments will only appear where needed
colAnn <- HeatmapAnnotation(
	df = ann,
	which = 'col', # 'col' (samples) or 'row' (gene) annotation?
	na_col = 'white', # default colour for any NA values in the annotation data-frame, 'ann'
	col = colours,
	annotation_height = 0.6,
	annotation_width = unit(1, 'cm'),
	gap = unit(1, 'mm'),
	annotation_legend_param = list(
	Pathotype = list(
		nrow = 4, # number of rows across which the legend will be arranged
		title = 'Pathotype',
		title_position = 'topcenter',
		legend_direction = 'vertical',
		title_gp = gpar(fontsize = 12, fontface = 'bold'),
		labels_gp = gpar(fontsize = 12, fontface = 'bold')
	),
	CD3 = list(
		nrow = 5,
		title = 'CD3',
		title_position = 'topcenter',
		legend_direction = 'vertical',
		title_gp = gpar(fontsize = 12, fontface = 'bold'),
		labels_gp = gpar(fontsize = 12, fontface = 'bold')
	),
	CD20 = list(
		nrow = 5,
		title = 'CD20',
		title_position = 'topcenter',
		legend_direction = 'vertical',
		title_gp = gpar(fontsize = 12, fontface = 'bold'),
		labels_gp = gpar(fontsize = 12, fontface = 'bold')
	),
	CD68L = list(
		nrow = 5,
		title = 'CD68L',
		title_position = 'topcenter',
		legend_direction = 'vertical',
		title_gp = gpar(fontsize = 12, fontface = 'bold'),
		labels_gp = gpar(fontsize = 12, fontface = 'bold')
	),
	CD68SL = list(
		nrow = 5,
		title = 'CD68SL',
		title_position = 'topcenter',
		legend_direction = 'vertical',
		title_gp = gpar(fontsize = 12, fontface = 'bold'),
		labels_gp = gpar(fontsize = 12, fontface = 'bold')
	),
	CD138 = list(
		nrow = 5,
		title = 'CD138',
		title_position = 'topcenter',
		legend_direction = 'vertical',
		title_gp = gpar(fontsize = 12, fontface = 'bold'),
		labels_gp = gpar(fontsize = 12, fontface = 'bold')))
)

boxplotCol <- HeatmapAnnotation(
	boxplot = anno_boxplot(
		heat,
		border = FALSE,
		gp = gpar(fill = '#CCCCCC'),
		pch = '.',
		size = unit(2, 'mm'),
		axis = TRUE,
		axis_param = list(
		gp = gpar(fontsize = 12),
		side = 'left')
	),
	annotation_width = unit(c(2.0), 'cm'),
	which = 'col'
)

boxplotRow <- HeatmapAnnotation(
	boxplot = row_anno_boxplot(
		heat,
		border = FALSE,
		gp = gpar(fill = '#CCCCCC'),
		pch = '.',
		size = unit(2, 'mm'),
		axis = TRUE,
		axis_param = list(
			gp = gpar(fontsize = 12),
			side = 'top')
		),
		annotation_width = unit(c(2.0), 'cm'),
		which = 'row'
)

genelabels <- rowAnnotation(
	Genes = anno_mark(
		at = seq(1, nrow(heat), 40),
		labels = rownames(heat)[seq(1, nrow(heat), 40)],
		labels_gp = gpar(fontsize = 10, fontface = 'bold'),
		padding = 0.75
	),
	width = unit(2.0, 'cm') +
		max_text_width(
			rownames(heat)[seq(1, nrow(heat), 40)],
			gp = gpar(fontsize = 10,  fontface = 'bold')
		)
)

pamClusters <- cluster::pam(heat, k = 4) # pre-select k = 4 centers
pamClusters$clustering <- paste0('Cluster ', pamClusters$clustering)

# fix order of the clusters to have 1 to 4, top to bottom
pamClusters$clustering <- factor(pamClusters$clustering,
	levels = c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4')
)

hmap <- Heatmap(heat,

	# split the genes / rows according to the PAM clusters
	split = pamClusters$clustering,
	cluster_row_slices = FALSE,

	name = 'z_score',

	col = colorRamp2(myBreaks, myCol),

	# parameters for the colour-bar that represents gradient of expression
	heatmap_legend_param = list(
		title = "Gene\nZ-\nscore",
		color_bar = 'continuous',
		legend_direction = 'vertical',
		legend_width = unit(8, 'cm'),
		legend_height = unit(5.0, 'cm'),
		title_position = 'topcenter',
		title_gp=gpar(fontsize = 12, fontface = 'bold'),
		labels_gp=gpar(fontsize = 12, fontface = 'bold')
	),

	# row (gene) parameters
	cluster_rows = TRUE,
	show_row_dend = TRUE,
	#row_title = 'Statistically significant genes',
	row_title_side = 'left',
	row_title_gp = gpar(fontsize = 12,  fontface = 'bold'),
	row_title_rot = 90,
	show_row_names = FALSE,
	row_names_gp = gpar(fontsize = 10, fontface = 'bold'),
	row_names_side = 'left',
	row_dend_width = unit(25,'mm'),

	# column (sample) parameters
	cluster_columns = TRUE,
	show_column_dend = TRUE,
	column_title = '',
	column_title_side = 'bottom',
	column_title_gp = gpar(fontsize = 12, fontface = 'bold'),
	column_title_rot = 0,
	show_column_names = FALSE,
	column_names_gp = gpar(fontsize = 10, fontface = 'bold'),
	column_names_max_height = unit(10, 'cm'),
	column_dend_height = unit(25,'mm'),

	# cluster methods for rows and columns
	clustering_distance_columns = function(x) as.dist(1 - cor(t(x))),
	clustering_method_columns = 'ward.D2',
	clustering_distance_rows = function(x) as.dist(1 - cor(t(x))),
	clustering_method_rows = 'ward.D2',

	# specify top and bottom annotations
	top_annotation = colAnn,
	bottom_annotation = boxplotCol
)

ht = draw(hmap + genelabels,
	heatmap_legend_side = 'left',
	annotation_legend_side = 'right',
	row_sub_title_side = 'left')

ht_shiny(ht, width1 = 900, height = 1200)

# title: Visualize cell heterogeneity from single cell RNASeq. This is from Supplementary S2 of the ComplexHeatmap paper. https://github.com/jokergoo/supplementary/tree/master/ComplexHeatmap-supplementary1-4

library(circlize)
library(ComplexHeatmap)
library(GetoptLong)

expr = read.table("https://raw.githubusercontent.com/jokergoo/supplementary/master/ComplexHeatmap-supplementary1-4/supplS2_scRNASeq/mouse_scRNAseq_corrected.txt", sep = "\t", header = TRUE)
expr = expr[!duplicated(expr[[1]]), ]
rownames(expr) = expr[[1]]
expr = expr[-1]
expr = as.matrix(expr)

library(cola)
mat = adjust_matrix(expr)
s = ATC(mat)
mat2 = mat[order(s, decreasing= TRUE)[1:500], , drop = FALSE]
base_mean = rowMeans(mat2)

mat2 = t(scale(t(mat2)))

cc = readRDS(url("https://github.com/jokergoo/supplementary/raw/master/ComplexHeatmap-supplementary1-4/supplS2_scRNASeq/mouse_cell_cycle_gene.rds"))
ccl = rownames(mat2) %in% cc
cc_gene = rownames(mat2)[ccl & base_mean > quantile(base_mean, 0.25)]

rp = readRDS(url("https://github.com/jokergoo/supplementary/raw/master/ComplexHeatmap-supplementary1-4/supplS2_scRNASeq/mouse_ribonucleoprotein.rds"))
rpl = rownames(mat2) %in% rp

ht_list = Heatmap(mat2, col = colorRamp2(c(-1.5, 0, 1.5), c("blue", "white", "red")), 
    show_row_names = FALSE, name = "scaled_expr", column_title = qq("relative expression for @{nrow(mat2)} genes"),
    show_column_names = FALSE, width = unit(8, "cm"),
    heatmap_legend_param = list(title = "Scaled expr")) +
    Heatmap(base_mean, name = "base_expr", show_row_names = FALSE, width = unit(5, "mm"),
        heatmap_legend_param = list(title = "Base expr")) +
    Heatmap(rpl + 0, name = "ribonucleoprotein", col = c("0" = "white", "1" = "purple"), 
        show_heatmap_legend = FALSE, width = unit(5, "mm")) +
    Heatmap(ccl + 0, name = "cell_cycle", col = c("0" = "white", "1" = "red"), 
        show_heatmap_legend = FALSE, width = unit(5, "mm")) +
    rowAnnotation(link = anno_mark(at = which(ccl & base_mean > quantile(base_mean, 0.25)), 
        labels = cc_gene, labels_gp = gpar(fontsize = 10), padding = 0.5), 
        width = unit(1, "cm") + max_text_width(cc_gene, gp = gpar(fontsize = 8))) +
    Heatmap(cor(t(mat2)), name = "cor", col = colorRamp2(c(-1, 0, 1), c("green", "white", "red")), 
        show_row_names = FALSE, show_column_names = FALSE, row_dend_side = "right", 
        show_column_dend = FALSE, column_title = "pairwise correlation between genes",
        heatmap_legend_param = list(title = "Correlation"))
ht_list = draw(ht_list, main_heatmap = "cor")

ht_shiny(ht_list, width1 = 900, height1 = 600)


# title: Correlations between methylation, expression and other genomic features. This is from Supplementary S3 of the ComplexHeatmap paper. https://github.com/jokergoo/supplementary/tree/master/ComplexHeatmap-supplementary1-4

library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)

res_list = readRDS(url("https://github.com/jokergoo/supplementary/raw/master/ComplexHeatmap-supplementary1-4/supplS3_methylation/meth.rds"))
type = res_list$type
mat_meth = res_list$mat_meth
mat_expr = res_list$mat_expr
direction = res_list$direction
cor_pvalue = res_list$cor_pvalue
gene_type = res_list$gene_type
anno_gene = res_list$anno_gene
dist = res_list$dist
anno_enhancer = res_list$anno_enhancer

column_tree = hclust(dist(t(mat_meth)))

ht_global_opt(legend_title_gp = gpar(fontsize = 8, fontface = "bold"), 
    legend_labels_gp = gpar(fontsize = 8), heatmap_column_names_gp = gpar(fontsize = 8))

ha = HeatmapAnnotation(df = data.frame(type = type), 
    col = list(type = c("Tumor" = "pink", "Control" = "royalblue")))
ha2 = HeatmapAnnotation(df = data.frame(type = type), 
    col = list(type = c("Tumor" = "pink", "Control" = "royalblue")), 
    show_legend = FALSE)

ht_list = Heatmap(mat_meth, name = "methylation", col = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red")),
    cluster_columns = column_tree, column_dend_reorder = FALSE, 
    top_annotation = ha, km = 5, column_title = "Methylation", column_title_gp = gpar(fontsize = 10), 
    row_title_gp = gpar(fontsize = 10)) +
    Heatmap(direction, name = "direction", col = c("hyper" = "red", "hypo" = "blue")) +
    Heatmap(mat_expr[, column_tree$order], name = "expression", 
        col = colorRamp2(c(-2, 0, 2), c("green", "white", "red")), cluster_columns = FALSE, 
        top_annotation = ha2, column_title = "Expression", column_title_gp = gpar(fontsize = 10)) +
    Heatmap(cor_pvalue, name = "-log10(cor_p)", col = colorRamp2(c(0, 2, 4), c("white", "white", "red"))) +
    Heatmap(gene_type, name = "gene type", col = structure(brewer.pal(length(unique(gene_type)), "Set3"), 
        names = unique(gene_type))) +
    Heatmap(anno_gene, name = "anno_gene", col = structure(brewer.pal(length(unique(anno_gene)), "Set1"), 
        names = unique(anno_gene))) +
    Heatmap(dist, name = "dist_tss", col = colorRamp2(c(0, 10000), c("black", "white"))) +
    Heatmap(anno_enhancer, name = "anno_enhancer", col = colorRamp2(c(0, 1), c("white", "orange")), 
        cluster_columns = FALSE, column_title = "Enhancer", column_title_gp = gpar(fontsize = 10))

ht_list = draw(ht_list, newpage = FALSE, column_title = "Comprehensive correspondence between methylation, expression and other genomic features", 
    column_title_gp = gpar(fontsize = 12, fontface = "bold"), heatmap_legend_side = "bottom")

ht_shiny(ht_list, width1 = 700, height1 = 800)

# title: A single shiny app with two interactive heatmaps applications.

m = matrix(rnorm(100*100), 100)
ht1 = Heatmap(m, col = c("white", "blue"))
ht1 = draw(ht1)
ht2 = Heatmap(m, col = c("white", "red"))
ht2 = draw(ht2)

ui = fluidPage(
    h3("The first heatmap"),
    InteractiveComplexHeatmapOutput("heatmap_1"),
    hr(),
    h3("The second heatmap"),
    InteractiveComplexHeatmapOutput("heatmap_2")
)

server = function(input, output, session) {
    MakeInteractiveComplexHeatmap(ht1, input, output, session, "heatmap_1")
    MakeInteractiveComplexHeatmap(ht2, input, output, session, "heatmap_2")
}

shinyApp(ui, server)

# title: Self-define the output.

m = matrix(rnorm(100*100), 100)
rownames(m) = paste0("R", 1:100)
colnames(m) = paste0("C", 1:100)
ht = Heatmap(m)

ui = fluidPage(
    InteractiveComplexHeatmapOutput(output_div = FALSE),
    htmlOutput("test")
)

click_action = function(df, output) {
	output[["test"]] = renderUI({
		HTML(qq("<p style='background-color:#FF8080;color:white;padding:5px;'>You have clicked on heatmap @{df$heatmap}, row @{df$row_index}, column @{df$column_index}</p>"))
	})
}

library(kableExtra)
brush_action = function(df, output) {
	row_index = unlist(df$row_index)
	column_index = unlist(df$column_index)
	output[["test"]] = renderUI({
		HTML(kable_styling(kbl(m[row_index, column_index, drop = FALSE], digits = 2, format = "html")))
	})
}

server = function(input, output, session) {
    MakeInteractiveComplexHeatmap(ht, input, output, session,
    	click_action = click_action, brush_action = brush_action,
    	default_click_action = FALSE, default_brush_action = FALSE)
}

shinyApp(ui, server)

# title: Self-define the output. A more complicated example.

library(EnrichedHeatmap)
load(system.file("extdata", "chr21_test_data.RData", package = "EnrichedHeatmap"))

gene_id = names(rpkm)[1:100]
gene_id = gsub("\\.\\d+$", "", gene_id)

library(org.Hs.eg.db)
query = select(org.Hs.eg.db, keys = gene_id, columns = c("SYMBOL", "REFSEQ", "UNIPROT"), keytype= "ENSEMBL")
query = split(query, query[, 1])

n = length(query)
m = matrix(rnorm(n*n), n)
rownames(m) = names(query)

ht = Heatmap(m, show_row_names = FALSE)

ui = fluidPage(
    InteractiveComplexHeatmapOutput(),
    htmlOutput("gene_info")
)

click_action = function(df, output) {
	output[["gene_info"]] = renderUI({
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

	})
}

server = function(input, output, session) {
    MakeInteractiveComplexHeatmap(ht, input, output, session, 
    	click_action = click_action)
}

shinyApp(ui, server)


# title: Integrate in an interactive R Markdown document



