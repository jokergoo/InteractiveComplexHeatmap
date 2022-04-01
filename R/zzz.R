.onAttach = function(libname, pkgname) {
    ComplexHeatmap::ht_opt(save_last = TRUE)

    version = packageDescription(pkgname, fields = "Version")

    msg = paste0("========================================
", pkgname, " version ", version, "
Bioconductor page: http://bioconductor.org/packages/InteractiveComplexHeatmap/
Github page: https://github.com/jokergoo/InteractiveComplexHeatmap

If you use it in published research, please cite:
Gu, Z. Make Interactive Complex Heatmaps in R, 2021, Bioinformatics

This message can be suppressed by:
  suppressPackageStartupMessages(library(InteractiveComplexHeatmap))
========================================
")

    packageStartupMessage(msg)
}

