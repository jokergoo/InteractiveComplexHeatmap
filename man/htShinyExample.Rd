\name{htShinyExample}
\alias{htShinyExample}
\title{
Examples of the interactive complex heatmaps
}
\description{
Examples of the interactive complex heatmaps
}
\usage{
htShinyExample(which)
}
\arguments{

  \item{which}{An integer of which example to use. The list of all examples can be obtained by executing \code{\link{htShinyExample}} with no argument.}

}
\details{
The source code of all examples are in \code{systm.file("examples", "examples.R")}.

}
\value{
A Shiny app object.
}
\examples{
htShinyExample()
if(interactive()) {
    htShinyExample(4.2)
}
}
