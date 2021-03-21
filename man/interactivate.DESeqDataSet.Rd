\name{interactivate.DESeqDataSet}
\alias{interactivate.DESeqDataSet}
\title{
Visualize DESeq2 result in an interactive Shiny app
}
\description{
Visualize DESeq2 result in an interactive Shiny app
}
\usage{
\method{interactivate}{DESeqDataSet}(x, res = DESeq2::results(x), seed = 123, ...)
}
\arguments{

  \item{x}{A \code{\link[DESeq2]{DESeqDataSet}} class object. It is normally returned by \code{\link[DESeq2]{DESeq}}.}
  \item{res}{The object returned by \code{\link[DESeq2]{results}}.}
  \item{seed}{Random seed. It is mainly set for the random colors of annotations.}
  \item{...}{Other arguments.}

}
\examples{
if(interactive()) {
    require(airway)
    data(airway)
    se = airway

    require(DESeq2)
    dds = DESeqDataSet(se, design = ~ dex)
    keep = rowSums(counts(dds)) >= 10
    dds = dds[keep, ]
    dds$dex = relevel(dds$dex, ref = "untrt")
    dds = DESeq(dds)

    interactivate(dds)
}
}
