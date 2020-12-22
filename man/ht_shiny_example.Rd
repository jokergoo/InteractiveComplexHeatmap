\name{ht_shiny_example}
\alias{ht_shiny_example}
\title{
Examples of the interactive complex heatmaps
}
\description{
Examples of the interactive complex heatmaps
}
\usage{
ht_shiny_example(which)
}
\arguments{

  \item{which}{An integer of which example to use. The list of all examples can be obtained by executing \code{\link{ht_shiny_example}} with no argument.}

}
\details{
The source code of all examples are in \code{systm.file("examples", "examples.R")}.
}
\value{
A shiny app object.
}
\examples{
ht_shiny_example()
if(interactive()) {
    ht_shiny_example(4)
}
}
