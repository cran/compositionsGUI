guiCheckData <-
function(d, compositionalOnly = FALSE) {
	if(!is.matrix(d) && !is.data.frame(d) && !is.acomp(d)) stop(paste("Invalid data type:", class(d)))
	if(nrow(d) < 2) stop("Less than 2 rows")
	if(ncol(d) < 2) stop("Less than 2 columns")

	if(compositionalOnly && (sum(lapply(d, class) == "numeric") + sum(lapply(d, class) == "integer")) != ncol(d)) {
		stop("Not all columns are numeric")
	}

	return(TRUE)
}

