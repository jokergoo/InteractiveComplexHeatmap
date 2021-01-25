# On public datasets

####################################################################
# title: An example from Lewis et al 2019. GitHub repo: https://github.com/kevinblighe/E-MTAB-6141

suppressPackageStartupMessages(require(RColorBrewer))
suppressPackageStartupMessages(require(ComplexHeatmap))
suppressPackageStartupMessages(require(circlize))
suppressPackageStartupMessages(require(digest))
suppressPackageStartupMessages(require(cluster))

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

htShiny(ht, width1 = 900, height1 = 1200)

####################################################################
# title: Visualize cell heterogeneity from single cell RNASeq. It is from Supplementary S2 of the ComplexHeatmap paper. https://github.com/jokergoo/supplementary

suppressPackageStartupMessages(library(circlize))
suppressPackageStartupMessages(library(ComplexHeatmap))
suppressPackageStartupMessages(library(GetoptLong))

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
    show_column_names = FALSE, width = unit(1, "null"),
    heatmap_legend_param = list(title = "Scaled expr")) +
    Heatmap(base_mean, name = "base_expr", show_row_names = FALSE, width = unit(5, "mm"),
        heatmap_legend_param = list(title = "Base expr"), column_names_rot = 45) +
    Heatmap(rpl + 0, name = "ribonucleoprotein", col = c("0" = "white", "1" = "purple"), 
        show_heatmap_legend = FALSE, width = unit(5, "mm"), column_names_rot = 45) +
    Heatmap(ccl + 0, name = "cell_cycle", col = c("0" = "white", "1" = "red"), 
        show_heatmap_legend = FALSE, width = unit(5, "mm"), column_names_rot = 45) +
    rowAnnotation(link = anno_mark(at = which(ccl & base_mean > quantile(base_mean, 0.25)), 
        labels = cc_gene, labels_gp = gpar(fontsize = 10), padding = 0.5), 
        width = unit(1, "cm") + max_text_width(cc_gene, gp = gpar(fontsize = 8))) +
    Heatmap(cor(t(mat2)), name = "cor", col = colorRamp2(c(-1, 0, 1), c("green", "white", "red")), 
        show_row_names = FALSE, show_column_names = FALSE, row_dend_side = "right", 
        show_column_dend = FALSE, column_title = "pairwise correlation between genes",
        heatmap_legend_param = list(title = "Correlation"), width = unit(2, "null"))
ht_list = draw(ht_list, main_heatmap = "cor")

htShiny(ht_list, width1 = 900, height1 = 600, width2 = 500)


####################################################################
# title: Correlations between methylation, expression and other genomic features. It is from Supplementary S3 of the ComplexHeatmap paper. https://github.com/jokergoo/supplementary

suppressPackageStartupMessages(library(ComplexHeatmap))
suppressPackageStartupMessages(library(circlize))
suppressPackageStartupMessages(library(RColorBrewer))

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

htShiny(ht_list, width1 = 700, height1 = 800)
