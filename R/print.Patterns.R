print.Patterns <-
function(x, ...){
	# identification of the pattern structure for missing values and DL
	# Matthias Templ, Nov 23, 2011
	if(sum(x$missingStructure$rsum) > 0) {
		cat("\n")
		cat(paste("The data include", sum(x$missingStructure$rsum), "missing values.\n"))
		cat("The missing data patterns are as follows:\n")
		print(x$missingStructure$tabcombPlus)
	} else {
		cat("No missing values are detected\n")
	}
	if(sum(x$DLStructure$rsum) > 0) {
		cat("\n")
		cat(paste("The data include", sum(x$DLStructure$rsum), "values below DL.\n"))
		cat("The DL data patterns are as follows:\n")
		print(x$DLStructure$tabcombPlus)
	} else {
		cat("\n")
		cat("No values under DL are detected\n")
	}	
	cat("\n")
}

