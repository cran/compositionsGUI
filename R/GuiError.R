GuiError <-
function(window, msg, object = NULL) {
	if(!is.null(object)) {
		separ = unlist(strsplit(object[1],":", fixed=TRUE))

		if(length(separ) > 1) d <- paste(separ[2:length(separ)], collapse = ":")
		else d = object[1]

		msg = paste(msg, d, sep=": ");
	}

	dialog <- gtkMessageDialog(window, "destroy-with-parent", "error", "close", msg)
	dialog$run()
	dialog$destroy()
}

