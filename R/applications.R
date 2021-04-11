# == title
# Generic function for interactivate an object in an interactive Shiny app
#
# == param
# -x An object.
# -... Other arguments.
#
interactivate = function (x, ...) {
    UseMethod("interactivate")
}

# == title
# Visualize DESeq2 result in an interactive Shiny app
#
# == param
# -x A `DESeq2::DESeqDataSet` class object. It is normally returned by `DESeq2::DESeq`.
# -res The object returned by `DESeq2::results`.
# -seed Random seed. It is mainly set for the random colors of annotations.
# -... Other arguments.
#
# == example
# if(interactive()) {
#     require(airway)
#     data(airway)
#     se = airway
#    
#     require(DESeq2)
#     dds = DESeqDataSet(se, design = ~ dex)
#     keep = rowSums(counts(dds)) >= 10
#     dds = dds[keep, ]
#     dds$dex = relevel(dds$dex, ref = "untrt")
#     dds = DESeq(dds)
#
#     interactivate(dds)
# }
interactivate.DESeqDataSet = function(x, res = DESeq2::results(x), seed = 123, ...) {

    dds = x
    res = as.data.frame(res)

    check_pkg("DESeq2")
    check_pkg("shinydashboard")
    check_pkg("DT")
    check_pkg("circlize")

    if(is.null(body(dds@dispersionFunction))) {
        dds = DESeq2::DESeq(dds)
    }

    full_mat = DESeq2::counts(dds, normalized = TRUE)
    anno = SummarizedExperiment::colData(dds)

    l = sapply(anno, function(x) (is.factor(x) || is.character(x)) && !any(duplicated(x))) | sapply(anno, function(x) length(unique(x)) == 1)
    anno = anno[, !l, drop = FALSE]

    env = new.env()

    qa = quantile(log10(res$baseMean + 1), 0.99)
    baseMean_col_fun = circlize::colorRamp2(c(0, qa/2, qa), c("blue", "white", "red"))
    
    qa = quantile(abs(abs(res$log2FoldChange)), 0.99)
    log2fc_col_fun = circlize::colorRamp2(c(-qa, 0, qa), c("green", "white", "red"))

    # Make the heatmap for differentially expressed genes under certain cutoffs.
    make_heatmap = function(fdr = 0.01, base_mean = 0, log2fc = 0, row_km = 0) {
    	l = res$padj <= fdr & res$baseMean >= base_mean & abs(res$log2FoldChange) >= log2fc; l[is.na(l)] = FALSE

    	if(sum(l) == 0) return(NULL)

    	m = full_mat[l, ]
        m = t(scale(t(m)))

        if(row_km < 0) {
            row_km = guess_best_km(m)
        }

        updateNumericInput(inputId = "km", value = row_km)

    	env$row_index = which(l)

        set.seed(seed)
    	ht = Heatmap(m, name = "z-score",
    	    top_annotation = HeatmapAnnotation(df = anno),
    	    show_row_names = FALSE, show_column_names = FALSE, row_km = row_km,
    	    column_title = paste0(sum(l), " significant genes with FDR < ", fdr),
    	    show_row_dend = FALSE) + 
    	    Heatmap(log10(res$baseMean[l]+1), show_row_names = FALSE, width = unit(5, "mm"),
    	        name = "log10(baseMean+1)", col = baseMean_col_fun, show_column_names = FALSE) +
    	    Heatmap(res$log2FoldChange[l], show_row_names = FALSE, width = unit(5, "mm"),
    	        name = "log2FoldChange", col = log2fc_col_fun, show_column_names = FALSE)
    	ht = draw(ht, merge_legend = TRUE)
        ht
    }

    # make the MA-plot with some genes highlighted
    make_maplot = function(res, highlight = NULL) {
        col = rep("#00000020", nrow(res))
        cex = rep(0.5, nrow(res))
        names(col) = rownames(res)
        names(cex) = rownames(res)
        if(!is.null(highlight)) {
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

    # make the volcano plot with some genes highlited
    make_volcano = function(res, highlight = NULL) {
        col = rep("#00000020", nrow(res))
        cex = rep(0.5, nrow(res))
        names(col) = rownames(res)
        names(cex) = rownames(res)
        if(!is.null(highlight)) {
            col[highlight] = "red"
            cex[highlight] = 1
        }
        x = res$log2FoldChange
        y = -log10(res$padj)
        col[col == "red" & x < 0] = "darkgreen"
        par(mar = c(4, 4, 1, 1))

        suppressWarnings(
            plot(x, y, col = col, 
                pch = 16, 
                cex = cex,
                xlab = "log2 fold change", ylab = "-log10(FDR)")
        )
    }

    # A self-defined action to respond brush event. It updates the MA-plot, the volcano plot
    # and a table which contains DESeq2 results for the selected genes.
    brush_action = function(df, input, output, session) {
        
        row_index = unique(unlist(df$row_index))
        selected = env$row_index[row_index]
            
        output[["ma_plot"]] = renderPlot({
            make_maplot(res, selected)
        })

        output[["ma_plot_selected"]] = renderUI({
            req(input$ma_plot_click)
            res2 = res
            res2$index = 1:nrow(res2)
            res2$log2FoldChange[res2$log2FoldChange > 2] = 2
            res2$log2FoldChange[res2$log2FoldChange < -2] = -2
            df = nearPoints(res2, input$ma_plot_click, xvar = "baseMean", yvar = "log2FoldChange")
            df = df[df$index %in% selected, , drop = FALSE]
            if(nrow(df) == 0) {
                return(NULL)
            } else {
                df = res[df$index, , drop = FALSE]
                gene = rownames(df)
                HTML(qq("
<pre>
Gene: @{gene}
baseMean: @{df[1, 'baseMean']}
log2FoldChange: @{df[1, 'log2FoldChange']}
lfcSE: @{df[1, 'lfcSE']},
stat: @{df[1, 'stat']}
pvalue: @{df[1, 'pvalue']}
FDR: @{df[1, 'padj']}</pre>
"))
            }
        })

        output[["volcano_plot"]] = renderPlot({
            make_volcano(res, selected)
        })

        output[["volcano_plot_selected"]] = renderUI({
            req(input$volcano_plot_click)
            res2 = res
            res2$index = 1:nrow(res2)
            res2$logfdr = -log10(res2$padj)
            df = nearPoints(res2, input$volcano_plot_click, xvar = "log2FoldChange", yvar = "logfdr")
            df = df[df$index %in% selected, , drop = FALSE]
            if(nrow(df) == 0) {
                return(NULL)
            } else {
                df = res[df$index, , drop = FALSE]
                gene = rownames(df)
                HTML(qq("
<pre>
Gene: @{gene}
baseMean: @{df[1, 'baseMean']}
log2FoldChange: @{df[1, 'log2FoldChange']}
lfcSE: @{df[1, 'lfcSE']},
stat: @{df[1, 'stat']}
pvalue: @{df[1, 'pvalue']}
FDR: @{df[1, 'padj']}</pre>
"))
            }
        })

        output[["res_table"]] = DT::renderDT(
            DT::formatRound(DT::datatable(res[selected, c("baseMean", "log2FoldChange", "padj")], rownames = TRUE), columns = 1:3, digits = 3)
        )

        output[["note"]] = renderUI({
        	if(!is.null(df)) {
        		HTML(qq("<p>Row indices captured in <b>Output</b> only correspond to the matrix of the differential genes. To get the row indices in the original matrix,  you need to perform:</p>
    <pre>
l = res$padj <= @{input$fdr} & 
    res$baseMean >= @{input$base_mean} & 
    abs(res$log2FoldChange) >= @{input$log2fc}
l[is.na(l)] = FALSE
which(l)[row_index]</pre>
    <p>where <code>res</code> is the complete data frame from DESeq2 analysis and <code>row_index</code> is the <code>row_index</code> column captured from the code in <b>Output</b>.</p>"))
        	}
        })
    }

    click_action = function(df, input, output, session) {
        row_index = unique(unlist(df$row_index))
        selected = env$row_index[row_index]
        
        output[["ma_plot"]] = renderPlot({
            make_maplot(res, selected)
        })

        output[["ma_plot_selected"]] = renderUI({
            req(input$ma_plot_click)
            res2 = res
            res2$index = 1:nrow(res2)
            res2$log2FoldChange[res2$log2FoldChange > 2] = 2
            res2$log2FoldChange[res2$log2FoldChange < -2] = -2
            df = nearPoints(res2, input$ma_plot_click, xvar = "baseMean", yvar = "log2FoldChange")
            df = df[df$index %in% selected, , drop = FALSE]
            if(nrow(df) == 0) {
                return(NULL)
            } else {
                df = res[df$index, , drop = FALSE]
                gene = rownames(df)
                HTML(qq("
<pre>
Gene: @{gene}
baseMean: @{df[1, 'baseMean']}
log2FoldChange: @{df[1, 'log2FoldChange']}
lfcSE: @{df[1, 'lfcSE']},
stat: @{df[1, 'stat']}
pvalue: @{df[1, 'pvalue']}
FDR: @{df[1, 'padj']}</pre>
"))
            }
        })

        output[["volcano_plot"]] = renderPlot({
            make_volcano(res, selected)
        })

        output[["volcano_plot_selected"]] = renderUI({
            req(input$volcano_plot_click)
            res2 = res
            res2$index = 1:nrow(res2)
            res2$logfdr = -log10(res2$padj)
            df = nearPoints(res2, input$volcano_plot_click, xvar = "log2FoldChange", yvar = "logfdr")
            df = df[df$index %in% selected, , drop = FALSE]
            if(nrow(df) == 0) {
                return(NULL)
            } else {
                df = res[df$index, , drop = FALSE]
                gene = rownames(df)
                HTML(qq("
<pre>
Gene: @{gene}
baseMean: @{df[1, 'baseMean']}
log2FoldChange: @{df[1, 'log2FoldChange']}
lfcSE: @{df[1, 'lfcSE']},
stat: @{df[1, 'stat']}
pvalue: @{df[1, 'pvalue']}
FDR: @{df[1, 'padj']}</pre>
"))
            }
        })

        output[["res_table"]] = DT::renderDT(
            DT::formatRound(DT::datatable(res[selected, c("baseMean", "log2FoldChange", "padj")], rownames = TRUE), columns = 1:3, digits = 3)
        )
    }

    # The dashboard body contains three columns:
    # 1. the original heatmap
    # 2. the sub-heatmap and the default output
    # 3. the self-defined output
    body = shinydashboard::dashboardBody(
        fluidRow(
            column(width = 4,
                shinydashboard::box(title = "Differential heatmap", width = NULL, solidHeader = TRUE, status = "primary",
                    originalHeatmapOutput("ht_deseq2", height = 800, containment = TRUE)
                )
            ),
            column(width = 4,
            	id = "column2",
                shinydashboard::box(title = "Sub-heatmap", width = NULL, solidHeader = TRUE, status = "primary",
                    subHeatmapOutput("ht_deseq2", title = NULL, containment = TRUE)
                ),
                shinydashboard::box(title = "Output", width = NULL, solidHeader = TRUE, status = "primary",
                    HeatmapInfoOutput("ht_deseq2", title = NULL)
                ),
                shinydashboard::box(title = "Note", width = NULL, solidHeader = TRUE, status = "primary",
                    htmlOutput("note")
                ),
            ),
            column(width = 4,
                shinydashboard::box(title = "MA-plot", width = NULL, solidHeader = TRUE, status = "primary",
                    p("Click on the highlighted point to see its related information."),
                    plotOutput("ma_plot", click = "ma_plot_click"),
                    htmlOutput("ma_plot_selected")
                ),
                shinydashboard::box(title = "Volcano plot", width = NULL, solidHeader = TRUE, status = "primary",
                    p("Click on the highlighted point to see its related information."),
                    plotOutput("volcano_plot", click = "volcano_plot_click"),
                    htmlOutput("volcano_plot_selected")
                ),
                shinydashboard::box(title = "Result table of the selected genes", width = NULL, solidHeader = TRUE, status = "primary",
                    DT::DTOutput("res_table")
                )
            )
        )
    )

    # Side bar contains settings for certain cutoffs to select significant genes.
    ui = shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(title = "DESeq2 results"),
        shinydashboard::dashboardSidebar(
            tags$label(HTML(qq("Comparison: <code style='font-weight:normal;'>@{paste(as.character(dds@design), collapse = ' ')}</code>")), class = "shiny-input-container", style = "font-size:1.2em;"),
        	hr(style="margin:2px;"),
            selectInput("fdr", label = "Cutoff for FDRs:", c("0.001" = 0.001, "0.01" = 0.01, "0.05" = 0.05)),
        	numericInput("base_mean", label = "Minimal base mean:", value = 0),
        	numericInput("log2fc", label = "Minimal abs(log2 fold change):", value = 0),
            numericInput("km", label = "Number of k-means groups. Set to 0 to suppress k-means clustering:", value = -1),
        	actionButton("filter", label = "Generate heatmap")
        ),
        body
    )

    # makeInteractiveComplexHeatmap() is put inside observeEvent() so that changes on the cutoffs can regenerate the heatmap.
    server = function(input, output, session) {
    	observeEvent(input$filter, {

            dev.null()
    		ht = make_heatmap(fdr = as.numeric(input$fdr), base_mean = input$base_mean, log2fc = input$log2fc,
                row_km = input$km)
            dev.off2()

    		if(!is.null(ht)) {
    		    makeInteractiveComplexHeatmap(input, output, session, ht, "ht_deseq2",
    		        brush_action = brush_action, click_action = click_action)
    		} else {
                # The ID for the heatmap plot is encoded as @{heatmap_id}_heatmap, thus, it is ht_heatmap here.
    			output$ht_heatmap = renderPlot({
    				grid.newpage()
    				grid.text("No row exists after filtering.")
    			})
    		}
    	}, ignoreNULL = FALSE)
    }

    shinyApp(ui, server)
}

# == title
# Interactive Shiny application for 2D density distribution
#
# == param
# -x A numeric vector.
# -y A numeric vector.
# -... All pass to `ks::kde`.
#
# == example
# if(interactive()) {
#     lt = readRDS(system.file("extdata", "2d_density_xy.rds", package = "InteractiveComplexHeatmap"))
#     interactivateDensity2D(lt$x, lt$y)
# }
interactivateDensity2D = function(x, y, ...) {
    fit = fit_2d_density(x, y, ...)
    interactivate(fit)
}


fit_2d_density = function(x, y, ...) {

    check_pkg("ks")

    if(missing(y) && identical(dim(x), 2)) {
        data = x
    } else {
        data = cbind(x, y)
    }

    l = is.na(x) | is.na(y)
    if(any(l)) {
        data = data[!l, , drop = FALSE]
        message_wrap(qq("remove @{sum(l)} data point@{ifelse(sum(l) == 1, '', 's')} that contain NAs."))
    }
    ks::kde(x = data, ...)
}

ht_2d_density = function(fit, ...) {

    m = fit$estimate
    m = t(m)
    m = m[rev(seq_len(nrow(m))), , drop = FALSE]
    ht = Heatmap(m, name = "Density",
        col = rev(brewer.pal(11, "Spectral")),
        show_row_names = FALSE, show_column_names = FALSE, 
        cluster_rows = FALSE, cluster_columns = FALSE,
        left_annotation = rowAnnotation(yaxis = anno_empty(border = FALSE)),
        bottom_annotation = HeatmapAnnotation(xaxis = anno_empty(border = FALSE)),
        ...
    )

    ht@heatmap_param$post_fun = function(ht) {
        decorate_heatmap_body("Density", {
            pushViewport(viewport(xscale = range(fit$eval.points[[1]]),
                                  yscale = range(fit$eval.points[[2]]), 
                                  clip = FALSE))
            grid.rect(gp = gpar(fill = NA))
            grid.xaxis(gp = gpar(fontsize = 8))
            grid.yaxis(gp = gpar(fontsize = 8))
            upViewport()
        })
    }
    ht
}

# == title
# Interactive Shiny application for 2D density distribution
#
# == param
# -x a ``kde`` object generated by `ks::kde`.
# -... Other arguments.
#
# == example
# if(interactive()) {
#     require(ks)
#     lt = readRDS(system.file("extdata", "2d_density_xy.rds", package = "InteractiveComplexHeatmap"))
#     data = cbind(lt$x, lt$y)
#     fit = kde(data)
#     interactivate(fit)
# }
interactivate.kde = function(x, ...) {

    fit = x

    x = fit$x[, 1]
    y = fit$x[, 2]

    ui = fluidPage(
        plotOutput("heatmap", width = 400, height = 400, brush = "heatmap_brush"),
        plotOutput("selected", width = 400, height = 400),
        tags$style(HTML("
            #heatmap, #selected {
                float:left;
                margin-top:20px;
            }
        "))
    )

    server = function(input, output, session) {

        ht_obj = reactiveVal(NULL)
        ht_pos_obj = reactiveVal(NULL)
        original_fit_obj = reactiveVal(NULL)

        output$heatmap = renderPlot({

            ht = ht_2d_density(fit, column_title = "2D density of the complete dataset")
            
            ht = draw(ht)
            ht_pos = htPositionsOnDevice(ht)

            ht_obj(ht)
            ht_pos_obj(ht_pos)
            original_fit_obj(fit)
        })

        observeEvent(input$heatmap_brush, {
            lt = getPositionFromBrush(input$heatmap_brush)

            selection = selectArea(ht_obj(), lt[[1]], lt[[2]], mark = FALSE, ht_pos = ht_pos_obj(), 
                verbose = FALSE)
            
            original_fit = original_fit_obj()
            xrange = range(original_fit$eval.points[[1]][unlist(selection$column_index)])
            yrange = range(rev(original_fit$eval.points[[2]])[unlist(selection$row_index)])
            
            l = x >= xrange[1] & x <= xrange[2] & y >= yrange[1] & y <= yrange[2]

            output$selected = renderPlot({
                if(sum(l) <= 2) {
                    grid.newpage()
                    grid.text("No enough data points.")
                } else {
                    fit = fit_2d_density(x[l], y[l], H = original_fit$H, gridtype = original_fit$gridtype, binned = original_fit$binned)
                    ht = ht_2d_density(fit, column_title = "2D density of the selected subset")
                    draw(ht)
                }
            })
        })
    }

    shinyApp(ui, server)

}
