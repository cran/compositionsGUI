Patterns <-
function(x){
	
	### Missing values ###
	resM <- missPatterns(x)
	
	### DL ###
	## initialize missings:
	y <- x
	y[is.na(y)] <- 999
	## set values below DL to missing:
	y[y < 0] <- NA 
	resDL <- missPatterns(y)
	
	res <- list("missingStructure" = resM, 
			    "DLStructure" = resDL)
	class(res) <- "Patterns"
	invisible(res)
}
