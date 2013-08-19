\name{sequence.Npositions}
\alias{sequence.Npositions}
\title{
Identify Sequence Position of Ambiguous Reads
}
\description{
Reads fastx files that have been converted to single strings, identifies the position of ambiguous reads ("N" reads), outputs positions as vector, list, and table, alongside a plot of the positions.
}
\usage{
sequence.Npositions(file)
}
\arguments{
  \item{file}{
a fasta or fastq file which has been converted to single line strings using fastx-toolkit bash scripts (\url{https://github.com/bbeesley/bash-projects/tree/master/fastx-toolkit}).
}
}
\details{
The function reads in the single line character strings from the input file as a character vector object, then creates a list from the vector. Each element of the list is a character vector giving the read positions (1 being the first read of the sequence) of ambiguous reads. A vector is produced giving all the positions of ambiguous reads. The position vector is then converted to a frequency table using \code{table}. The character vector form of the fastx file is then saved to the user's workspace, along with the list, vector, and frequency table forms of the ambiguous read positions. A plot is then loaded giving an overview of the data.
}
\value{
\item{fasta object}{A character vector, each element of which is a sequence from the input fastx file.}
\item{N positions list}{A list each element of which giving the positions of ambiguous reads in a sequence}
\item{N positions vector}{An integer vector giving the position of each ambiguous read.}
\item{N positions table}{An object of class \code{table} containing the ambiguous read positions of the input fastx file in the form of a frequency table.}
}
\author{
Bill Beesley\cr
\email{bbeesley@me.com}\cr
\url{https://github.com/bbeesley/}\cr
}
\seealso{
\code{\link{sequence.lengths}} for lengths of sequences.
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
sequence.Npositions(file="demoFasta.txt")

## Delete the demo fasta file
unlink("demoFasta.txt")
}
