#-------------------------------------------------------------------------------
# Convert Stata file to Cockpit file
# convert-spss version 1.0, 3 May 2010, by Jelke Bethlehem
# source("d://cockpit/test-export/export-stata.r")
#-------------------------------------------------------------------------------

 rm(list=ls())
 library(foreign)
  
#-------------------------------------------------------------------------------
# Read Stata file
#-------------------------------------------------------------------------------

 stata.data = read.dta('d://cockpit/test-export/gps.dta')
 n = length(stata.data[[1]])
  
#-------------------------------------------------------------------------------
# Specify variables to export
#-------------------------------------------------------------------------------
  
 model = c("URBAN", "AGE3", "PHONE", "MARRIED", "HHSIZE", "ETHNIC")
 rest  = c("GENDER", "REGION", "PNONNAT1", "ALLOWAN", "HASJOB", "HHTYPE", "RESPONSE")
 probs = "RESPROB"
 filename = "d://cockpit/test-export/gps"
  
#-------------------------------------------------------------------------------
# Export data 
#-------------------------------------------------------------------------------
 
 cat("Preparing data ...\n")
 f1 = paste(filename, ".dat", sep="")
 p1 = length(model)
 p2 = length(rest)
 p = p1 + p2
 p3 = p + 1
 data = matrix(rep(0, n * p3), n, p3)
 prob = rep(0, n)
 
 for (i in 1:p1)
 { col = stata.data[[model[i]]]
   data[,i] = as.integer(col)
 }
 for (i in 1:p2)
 { col = stata.data[[rest[i]]]
   data[,p1 + i] = as.integer(col)
 }
 prob = stata.data[[probs]] 
 data[, p3] = as.real(prob)
 ddd = t(data)

 cat("Writing data ...\n")
 write(ddd, file=f1, ncolumns=p3)
 
#-------------------------------------------------------------------------------
# Export metadata 
#-------------------------------------------------------------------------------

 cat("Writing metadata ...\n")
 f2 = paste(filename, ".rin", sep="")
 cat("Data", "\n", sep="", file=f2, append=FALSE)
 cat("  ", f1, "\n", sep="", file=f2, append=TRUE)
 cat("Variables:", p, "\n", sep="", file=f2, append=TRUE)
 for (i in 1:p1)
 { col = stata.data[[model[i]]]
   nam = model[i]
   lev = levels(col)
   lab = lev
   cts = length(lev)
   cod = 1:cts
   cat("  ", nam, ":", cts, ",1" , "\n", sep="", file=f2, append=TRUE)
   for (j in 1:cts)
   { cat("    ", cod[j], ":", lab[j], "\n", sep="", file=f2, append=TRUE) }
 }
 for (i in 1:p2)
 { col = stata.data[[rest[i]]]
   nam = rest[i]
   lev = levels(col)
   lab = lev
   cts = length(lev)
   cod = 1:cts
   cat("  ", nam, ":", cts, ",0" , "\n", sep="", file=f2, append=TRUE)
   for (j in 1:cts)
   { cat("    ", cod[j], ":", lab[j], "\n", sep="", file=f2, append=TRUE) }
 }

#-------------------------------------------------------------------------------
# End of program
#-------------------------------------------------------------------------------
 
  
