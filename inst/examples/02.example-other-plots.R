# On other plots and packages

####################################################################
# title: A density heatmap.

load(url("https://jokergoo.github.io/images/density_heatmap_dataset.RData"))
ht = densityHeatmap(mat2, ylim = c(0, 1), ylab = "Methylation")
ht = draw(ht)

htShiny(ht, width1 = 500)

####################################################################
# title: An oncoPrint.

# This example is directly copied from ComplexHeatmap book:
# https://jokergoo.github.io/ComplexHeatmap-reference/book/oncoprint.html#apply-to-cbioportal-dataset
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
ht = draw(ht)

htShiny(ht, width1 = 800)


####################################################################
# title: A UpSet plot.

movies = read.csv(system.file("extdata", "movies.csv", package = "UpSetR"), 
    header = TRUE, sep = ";")
m = make_comb_mat(movies, top_n_sets = 10)
m = m[comb_degree(m) > 0]
ht = UpSet(m)
ht = draw(ht)

htShiny(ht, width1 = 800)

####################################################################
# title: An interactive heatmap from pheatmap().

# The example is from pheatmap::pheatmap help page.
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
p = draw(p)

htShiny(p)

####################################################################
# title: An interactive heatmap from heatmap().

x  = as.matrix(mtcars)
rc = rainbow(nrow(x), start = 0, end = 0.3)
cc = rainbow(ncol(x), start = 0, end = 0.3)

## note `heatmap()` should be from ComplexHeatmap package
ht = ComplexHeatmap:::heatmap(x, col = cm.colors(256), scale = "column",
              RowSideColors = rc, ColSideColors = cc, margins = c(5,10),
              xlab = "specification variables", ylab =  "Car Models",
              main = "heatmap(<Mtcars data>, ..., scale = \"column\")")
ht = draw(ht)

htShiny(ht)


####################################################################
# title: An interactive heatmap from heatmap.2().

data(mtcars)
x = as.matrix(mtcars)

# note `heatmap.2()` should be from ComplexHeatmap package
ht = ComplexHeatmap:::heatmap.2(x, col = gplots::bluered, scale = "column", tracecol = "#303030")
ht = draw(ht)

htShiny(ht)

####################################################################
# title: A heatmap produced from tidyHeatmap package.

# The example is from tidyHeatmap GitHub readme.
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidyHeatmap))
mtcars_tidy <- 
    mtcars %>% 
    as_tibble(rownames="Car name") %>% 
    mutate_at(vars(-`Car name`, -hp, -vs), scale) %>%
    pivot_longer(cols = -c(`Car name`, hp, vs), names_to = "Property", values_to = "Value")

mtcars_heatmap <- 
    mtcars_tidy %>% 
        heatmap(`Car name`, Property, Value ) %>%
        add_tile(hp)

htShiny(mtcars_heatmap)


####################################################################
# title: Genome-scale heatmap.

suppressPackageStartupMessages(library(circlize))
suppressPackageStartupMessages(library(GenomicRanges))

chr_window = bin_genome("hg19")

#### the first is a numeric matrix #######
bed1 = generateRandomBed(nr = 1000, nc = 10)
gr1 = GRanges(seqnames = bed1[, 1], ranges = IRanges(bed1[, 2], bed1[, 3]))

num_mat = normalize_genomic_signals_to_bins(gr1, bed1[, -(1:3)])

#### the second is a character matrix ######
bed_list = lapply(1:10, function(i) {
    generateRandomBed(nr = 1000, nc = 1, 
        fun = function(n) sample(c("gain", "loss"), n, replace = TRUE))
})
char_mat = NULL
for(i in 1:10) {
    bed = bed_list[[i]]
    bed = bed[sample(nrow(bed), 20), , drop = FALSE]
    gr_cnv = GRanges(seqnames = bed[, 1], ranges = IRanges(bed[, 2], bed[, 3]))

    char_mat = cbind(char_mat, normalize_genomic_signals_to_bins(gr_cnv, bed[, 4]))
}

#### two numeric columns ##########
bed2 = generateRandomBed(nr = 100, nc = 2)
gr2 = GRanges(seqnames = bed2[, 1], ranges = IRanges(bed2[, 2], bed2[, 3]))

v = normalize_genomic_signals_to_bins(gr2, bed2[, 4:5])

##### a list of genes need to be marked
bed3 = generateRandomBed(nr = 40, nc = 0)
gr3 = GRanges(seqnames = bed3[, 1], ranges = IRanges(bed3[, 2], bed3[, 2]))
gr3$gene = paste0("gene_", 1:length(gr3))

mtch = as.matrix(findOverlaps(chr_window, gr3))
at = mtch[, 1]
labels = mcols(gr3)[mtch[, 2], 1]

##### order of the chromosomes ########
chr = as.vector(seqnames(chr_window))
chr_level = paste0("chr", c(1:22, "X", "Y"))
chr = factor(chr, levels = chr_level)

#### make the heatmap #######
subgroup = rep(c("A", "B"), each = 5)

ht_opt$TITLE_PADDING = unit(c(4, 4), "points")
ht_list = Heatmap(num_mat, name = "mat", col = colorRamp2(c(-1, 0, 1), c("green", "white", "red")),
        row_split = chr, cluster_rows = FALSE, show_column_dend = FALSE,
        column_split = subgroup, cluster_column_slices = FALSE,
        column_title = "numeric matrix",
        top_annotation = HeatmapAnnotation(subgroup = subgroup, annotation_name_side = "left"),
        row_title_rot = 0, row_title_gp = gpar(fontsize = 10), border = TRUE,
        row_gap = unit(0, "points")) +
    Heatmap(char_mat, name = "CNV", col = c("gain" = "red", "loss" = "blue"),
        border = TRUE, column_title = "character matrix") +
    rowAnnotation(label = anno_mark(at = at, labels = labels)) +
    rowAnnotation(pt = anno_points(v, gp = gpar(col = 4:5), pch = c(1, 16)), 
        width = unit(2, "cm")) +
    rowAnnotation(bar = anno_barplot(v[, 1], gp = gpar(col = ifelse(v[ ,1] > 0, 2, 3))), 
        width = unit(2, "cm"))
ht_list = draw(ht_list, merge_legend = TRUE)

htShiny(ht_list, width1 = 600, height1 = 700)

