\name{rand_mat}
\docType{data}
\alias{rand_mat}
\title{
A random matrix
}
\description{
A random matrix
}
\usage{
data(rand_mat)
}
\details{
Following code was used to generate \code{rand_mat}:

  \preformatted{
  set.seed(123)
  rand_mat = cbind(rbind(matrix(rnorm(20*20, mean = 1, sd = 0.5), nr = 20),
                  matrix(rnorm(20*20, mean = 0, sd = 0.5), nr = 20),
                  matrix(rnorm(20*20, mean = 0, sd = 0.5), nr = 20)),
            rbind(matrix(rnorm(20*20, mean = 0, sd = 0.5), nr = 20),
                  matrix(rnorm(20*20, mean = 1, sd = 0.5), nr = 20),
                  matrix(rnorm(20*20, mean = 0, sd = 0.5), nr = 20)),
            rbind(matrix(rnorm(20*20, mean = 0.5, sd = 0.5), nr = 20),
                  matrix(rnorm(20*20, mean = 0.5, sd = 0.5), nr = 20),
                  matrix(rnorm(20*20, mean = 1, sd = 0.5), nr = 20))
           ) + matrix(rnorm(60*60, sd = 0.5), nr = 60)
   colnames(rand_mat) = paste0("C", 1:60)
   rownames(rand_mat) = paste0("R", 1:60)  }
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
data(rand_mat)
rand_mat
}
