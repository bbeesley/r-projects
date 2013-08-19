sequence.Npositions <-
function(file) {
  baseName        <- sub("\\.[[:alpha:]]{3}", "", file)
  fasta           <- scan(file, what = "character()")
  splitFasta      <- strsplit(fasta, split = character())
  Npositions      <- lapply(splitFasta, function(read) {
    grep("N", read)
  })
  empties         <- sapply(Npositions, length) == 0
  NposVector      <- unlist(Npositions[!empties])
  hist(NposVector, 
       breaks = max(NposVector)/5, 
       col = "lightblue",
       main = "Histogram of Ambiguous Read Positions", 
       xlab = "Sequence Position of Ambiguous Read")
  assign(paste(baseName, ".fasta", sep = ""), 
         fasta, 
         envir = .GlobalEnv)
  assign(paste(baseName, "Npos", sep = ""), 
         Npositions, 
         envir = .GlobalEnv)
  assign(paste(baseName, "NposVector", sep = ""), 
         NposVector, 
         envir = .GlobalEnv)
}
