\name{fastxToolkit-package}
\alias{fastxToolkit-package}
\alias{fastxToolkit}
\docType{package}
\title{
Toolkit for Fasta and Fastq Files
}
\description{
Provides simple tools to perform various analyses on fasta and fastq files which have been processed with the bash scripts in fastx-toolkit \url{https://github.com/bbeesley/bash-projects/tree/master/fastx-toolkit}
}
\details{
\tabular{ll}{
Package: \tab fastxToolkit\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-08-19\cr
License: \tab GPL (>= 2)\cr
}
Use the bash scripts in fastx toolkit (\url{https://github.com/bbeesley/bash-projects/tree/master/fastx-toolkit}) to convert fasta or fastq files into single line character strings. Then perform analysis by running \code{\link{sequence.lengths}()} or \code{\link{sequence.Npositions}()} with the sequence text file output from the bash scripts as the argument.
}
\author{
Bill Beesley\cr
\email{bbeesley@me.com}\cr
\url{https://github.com/bbeesley/}\cr

Maintainer: Bill Beesley <bbeesley@me.com>
}
\keyword{ package }
\seealso{
\code{\link{sequence.lengths}} and \code{\link{sequence.Npositions}}
}
\examples{
## Make a fake fasta file with 500 sequences
fasta <- sapply(as.integer(pmin(sqrt((rcauchy(n=500, 
                                              location = 400, 
                                              scale = 2))^2), 
                                602)), function(n) {
  gsub(", ", "", toString(sample(x=c(rep(c("G", "A", "T", "C"), 
                                         150), 
                                     rep("N", 
                                         40)), 
                                 size=n, 
                                 replace=FALSE, 
                                 prob = c(rep(5000/164000, 600),
                                          rep(1/164000, 40)))))
})

## Write it to disk
writeLines(fasta, "demoFasta.txt")

## Run the function
sequence.lengths(file="demoFasta.txt")
sequence.Npositions(file="demoFasta.txt")

## Delete the demo fasta file
unlink("demoFasta.txt")
}
