\name{sequence.lengths}
\alias{sequence.lengths}
\title{
Calculate Read Length of Sequences
}
\description{
Measures read lengths of fastx sequences which have been converted to character strings, plots the results, and produces a vector of read lengths and a table of read lengths.
}
\usage{
sequence.lengths(file)
}
\arguments{
  \item{file}{
a fasta or fastq file which has been converted to single line strings using fastx-toolkit bash scripts (\url{https://github.com/bbeesley/bash-projects/tree/master/fastx-toolkit}).
}
}
\details{
The function reads in the single line character strings from the input file as a character vector object, then runs \code{nchar} on each element of the vector. The read lengths are then converted to a frequency table using \code{table}. The character vector form of the fastx file is then saved to the user's workspace, along with the vector and frequency table forms of the read lengths. A plot is then loaded giving an overview of the data.
}
\value{
\item{fasta object}{A character vector, each element of which is a sequence from the input fastx file.}
\item{fasta lengths}{An integer vector giving the length of each sequence from the input fastx file.}
\item{fasta length table}{An object of class \code{table} containing the sequence lengths of the input fastx file in the form of a frequency table.}
}
\references{
fastx-toolkit bash scripts are required for pre-processing. \url{https://github.com/bbeesley/bash-projects/tree/master/fastx-toolkit}
}
\author{
Bill Beesley\cr
\email{bbeesley@me.com}\cr
\url{https://github.com/bbeesley/}\cr
}
\seealso{
\code{\link{sequence.Npositions}} for positions of ambiguous reads.
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

## Delete the demo fasta file
unlink("demoFasta.txt")
}
