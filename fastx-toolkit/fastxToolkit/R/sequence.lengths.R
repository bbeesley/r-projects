sequence.lengths <-
function(file) {
  baseName <- sub("\\.[[:alpha:]]{3}", "", file)
  fasta           <- scan(file, what = "character()")
  seqLengths      <- nchar(fasta)
  hist(seqLengths, 
       breaks = max(seqLengths)/5, 
       col = "lightblue",
       main = "Histogram of Sequence Lengths", 
       xlab = "Bases")
  seqLengthTable  <- table(seqLengths)
  assign(paste(baseName, ".fasta", sep = ""), 
         fasta, 
         envir = .GlobalEnv)
  assign(paste(baseName, "Lengths", sep = ""), 
         seqLengths, 
         envir = .GlobalEnv)
  assign(paste(baseName, "LengthTable", sep = ""), 
         seqLengthTable, 
         envir = .GlobalEnv)
}
