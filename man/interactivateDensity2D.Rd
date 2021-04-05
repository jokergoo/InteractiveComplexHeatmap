\name{interactivateDensity2D}
\alias{interactivateDensity2D}
\title{
Interactive Shiny application for 2D density distribution
}
\description{
Interactive Shiny application for 2D density distribution
}
\usage{
interactivateDensity2D(x, y, ...)
}
\arguments{

  \item{x}{A numeric vector.}
  \item{y}{A numeric vector.}
  \item{...}{All pass to \code{\link[ks]{kde}}.}

}
\examples{
if(interactive()) {
    lt = readRDS(system.file("extdata", "2d_density_xy.rds", package = "InteractiveComplexHeatmap"))
    interactivateDensity2D(lt$x, lt$y)
}
}
