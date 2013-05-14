guiDataSummary <-
function() {
	try({
		if(!is.null(guiGetData("originalName"))) {
			original = get(guiGetData("originalName"), envir=.GlobalEnv)
			compositional = get(guiGetData("compName"), envir=.GlobalEnv)
			external = get(guiGetData("externalName"), envir=.GlobalEnv)

			missings <- Patterns(compositional)
			nas = sum(missings$missingStructure$rsum)
			dls = sum(missings$DLStructure$rsum)

			status <- paste0("rows: ", nrow(original),",  columns: ", ncol(compositional), ",  externals: ", ncol(external), ",  missings: ", nas, ",  under detection: ", dls)
			guiGetData("dataStatusLabel")$setText(status)
		} else {
			guiGetData("dataStatusLabel")$setText("no data loaded")
		}
	}, TRUE)
}

