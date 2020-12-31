.onAttach = function(libname, pkgname) {
    version = packageDescription(pkgname, fields = "Version")

  	msg = paste0("========================================
", pkgname, " version ", version, "
Bioconductor page: http://bioconductor.org/packages/InteractiveComplexHeatmap/
Github page: https://github.com/jokergoo/InteractiveComplexHeatmap

This message can be suppressed by:
  suppressPackageStartupMessages(library(InteractiveComplexHeatmap))
========================================
")	

    packageStartupMessage(msg)

    ComplexHeatmap::ht_opt(save_last = TRUE)
}
