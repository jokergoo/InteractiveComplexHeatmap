# Visualize DESeq2 result in an interactive Shiny app

Visualize DESeq2 result in an interactive Shiny app

## Usage

``` r
# S3 method for class 'DESeqDataSet'
interactivate(x, res = DESeq2::results(x), seed = 123, ...)
```

## Arguments

- x:

  A
  [`DESeq2::DESeqDataSet`](https://rdrr.io/pkg/DESeq2/man/DESeqDataSet.html)
  class object. It is normally returned by
  [`DESeq2::DESeq()`](https://rdrr.io/pkg/DESeq2/man/DESeq.html).

- res:

  The object returned by
  [`DESeq2::results()`](https://rdrr.io/pkg/DESeq2/man/results.html).

- seed:

  Random seed. It is mainly set for the random colors of annotations.

- ...:

  Other arguments.

## Examples

``` r
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
```
