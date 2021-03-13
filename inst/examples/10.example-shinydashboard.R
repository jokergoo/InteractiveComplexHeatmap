# Work with shinydashboard

##########################################
# title: Seperate the three UI components into three boxes.

library(shinydashboard)
m = matrix(rnorm(100), 10)
ht = Heatmap(m)

body = dashboardBody(
	fluidRow(
		box(title = "Original heatmap", width = 4, solidHeader = TRUE, status = "primary",
			mainHeatmapOutput("ht")
		),
		box(title = "Sub-heatmap", width = 4, solidHeader = TRUE, status = "primary",
			subHeatmapOutput("ht")
		),
		box(title = "Output", width = 4, solidHeader = TRUE, status = "primary",
			HeatmapInfoOutput("ht")
		)
	)
)

ui = dashboardPage(
	dashboardHeader(),
	dashboardSidebar(),
	body
)

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht, "ht")
}

shinyApp(ui, server)


##########################################
# title: A Shiny dashboard with two tabs

library(shinydashboard)
m1 = matrix(rnorm(100), 10)
ht1 = Heatmap(m1)

m2 = matrix(sample(letters[1:10], 100, replace = TRUE), 10)
ht2 = Heatmap(m2)

side_bar = dashboardSidebar(
	sidebarMenu(
		menuItem("A numeric heatmap", tabName = "numeric"),
		menuItem("A character heatmap", tabName = "character")
	)
)

single_heatmap_ui = function(heatmap_id) {
	fluidRow(
		box(title = "Original heatmap", width = 4, solidHeader = TRUE, status = "primary",
			mainHeatmapOutput(heatmap_id)
		),
		box(title = "Sub-heatmap", width = 4, solidHeader = TRUE, status = "primary",
			subHeatmapOutput(heatmap_id)
		),
		box(title = "Output", width = 4, solidHeader = TRUE, status = "primary",
			HeatmapInfoOutput(heatmap_id)
		)
	)
}

body = dashboardBody(
	tabItems(
		tabItem(tabName = "numeric", single_heatmap_ui("ht1")),
		tabItem(tabName = "character", single_heatmap_ui("ht2"))
	)
)

ui = dashboardPage(
	dashboardHeader(title = "InteractiveComplexHeatmap works with shinydashboard"),
	side_bar,
	body
)

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht1, "ht1")
    makeInteractiveComplexHeatmap(input, output, session, ht2, "ht2")
}

shinyApp(ui, server)


########################################################
# title: Only contains the main heatmap where output is floating

library(shinydashboard)
m = matrix(rnorm(100), 10)
ht = Heatmap(m)

body = dashboardBody(
	fluidRow(
		box(title = "Original heatmap", width = 4, solidHeader = TRUE, status = "primary",
			mainHeatmapOutput("ht", response = "click"),
			HeatmapInfoOutput("ht", output_ui_float = TRUE) # this line can be put anywhere
		)
	)
)

ui = dashboardPage(
	dashboardHeader(title = "InteractiveComplexHeatmap works with shinydashboard"),
	dashboardSidebar(),
	body
)

server = function(input, output, session) {
    makeInteractiveComplexHeatmap(input, output, session, ht, "ht")
}

shinyApp(ui, server)


#########################################################
# title: Visualize a DESeq2 results with shinydashboard.

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

library(ComplexHeatmap)
library(circlize)

env = new.env()

make_heatmap = function(fdr = 0.01, base_mean = 0, log2fc = 0) {
	l = res$padj <= fdr & res$baseMean >= base_mean & abs(res$log2FoldChange) >= log2fc; l[is.na(l)] = FALSE

	if(sum(l) == 0) return(NULL)

	m = counts(dds, normalized = TRUE)
	m = m[l, ]

	env$row_index = which(l)

	ht = Heatmap(t(scale(t(m))), name = "z-score",
	    top_annotation = HeatmapAnnotation(
	        dex = colData(dds)$dex,
	        sizeFactor = anno_points(colData(dds)$sizeFactor)
	    ),
	    show_row_names = FALSE, show_column_names = FALSE, row_km = 2,
	    column_title = paste0(sum(l), " significant genes with FDR < ", fdr),
	    show_row_dend = FALSE) + 
	    Heatmap(log10(res$baseMean[l]+1), show_row_names = FALSE, width = unit(5, "mm"),
	        name = "log10(baseMean+1)", show_column_names = FALSE) +
	    Heatmap(res$log2FoldChange[l], show_row_names = FALSE, width = unit(5, "mm"),
	        name = "log2FoldChange", show_column_names = FALSE,
	        col = colorRamp2(c(-2, 0, 2), c("green", "white", "red")))
	ht = draw(ht, merge_legend = TRUE)
}

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

library(DT)
brush_action = function(df, output) {
    
    row_index = unique(unlist(df$row_index))
    selected = env$row_index[row_index]
        
    output[["maplot"]] = renderPlot({
        make_maplot(res, selected)
    })

    output[["res_table"]] = renderDT(
        formatRound(datatable(res[selected, c("baseMean", "log2FoldChange", "padj")], rownames = TRUE), columns = 1:ncol(res), digits = 3)
    )
}

library(shinydashboard)
body = dashboardBody(
    fluidRow(
        column(width = 4,
            box(title = "Differential heatmap", width = NULL, solidHeader = TRUE, status = "primary",
                mainHeatmapOutput("ht", height = 800, containment = TRUE)
            )
        ),
        column(width = 4,
        	id = "column2",
            box(title = "Sub-heatmap", width = NULL, solidHeader = TRUE, status = "primary",
                subHeatmapOutput("ht", title = NULL, containment = TRUE)
            ),
            box(title = "Output", width = NULL, solidHeader = TRUE, status = "primary",
                HeatmapInfoOutput("ht", title = NULL)
            )
        ),
        column(width = 4,
            box(title = "MA-plot", width = NULL, solidHeader = TRUE, status = "primary",
                plotOutput("maplot")
            ),
            box(title = "Result table of the selected genes", width = NULL, solidHeader = TRUE, status = "primary",
                DTOutput("res_table")
            )
        )
    )
)

ui = dashboardPage(
    dashboardHeader(title = "DESeq2 results"),
    dashboardSidebar(
    	selectInput("cutoff", label = "Cutoff for FDRs:", c("0.05" = 0.05, "0.01" = 0.01, "0.001" = 0.001)),
    	numericInput("base_mean", label = "Minimal base mean:", value = 0),
    	numericInput("log2fc", label = "Minimal abs(log2 fold change):", value = 0),
    	actionButton("filter", label = "Generate heatmap")
    ),
    body
)

server = function(input, output, session) {
	observeEvent(input$filter, {
		ht = make_heatmap(fdr = as.numeric(input$cutoff), base_mean = input$base_mean, log2fc = input$log2fc)
		if(!is.null(ht)) {
		    makeInteractiveComplexHeatmap(input, output, session, ht, "ht",
		        brush_action = brush_action)
		} else {
			output$ht_heatmap = renderPlot({
				grid.newpage()
				grid.text("No row exists after filtering.")
			})
		}
	})
}

shinyApp(ui, server)



