\name{htShinyExample}
\alias{htShinyExample}
\title{
Examples of interactive complex heatmaps
}
\description{
Examples of interactive complex heatmaps
}
\usage{
htShinyExample(which)
}
\arguments{

  \item{which}{An index of which example to use. The list of all examples can be obtained by executing \code{\link{htShinyExample}} with no argument.}

}
\details{
In every example, there is a Shiny app opened, which also includes source code that generates this app.
}
\value{
A Shiny app object.
}
\examples{
# list all examples
htShinyExample()

if(interactive()) {
    htShinyExample(4.2)
}
}
