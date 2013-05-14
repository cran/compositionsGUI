GuiLoadFile <-
function(window) {
	# that ugly window to select file
	dialog <- gtkFileChooserDialogNew("Select input data file", NULL, "open",
					  "gtk-cancel", GtkResponseType["cancel"],
					  "gtk-open", GtkResponseType["accept"])

	guiSetHelp("loadfilefile", dialog)

	# and filters to let user filter files
	filter1 <- gtkFileFilterNew()
	filter1$setName("*.txt, *.csv");
	filter1$addPattern("*.txt")
	filter1$addPattern("*.csv")
	filter2 <-gtkFileFilterNew()
	filter2$setName("All files")
	filter2$addPattern("*")
	dialog$addFilter(filter1)
	dialog$addFilter(filter2)

	if(!is.null(guiGetData("loadfile_dir"))) dialog$setCurrentFolderUri(guiGetData("loadfile_dir"))

	# and get results
	res <- dialog$run()
	filename <- dialog$getFilename()
	guiSetData("loadfile_dir", dialog$getCurrentFolderUri())
	dialog$destroy()

	# and we do only if we have something
	if (res == GtkResponseType["accept"]) {
		# and some weird error
		if(nchar(filename) < 1) return(GuiError(window, "Empty filename"))

		# and we will try to speed the process up by selecting right function for particular extension
		extension = tail(unlist(strsplit(filename, "\\.")), 1)
		if(extension == "csv") funs = c("read.csv2", "read.csv", "read.delim", "read.table")
		else if(extension == "txt") funs = c("read.table", "read.csv2", "read.csv", "read.delim")
		else funs = c("read.table", "read.csv2", "read.csv", "read.delim")

		return(guiParseFile(window, filename, funs))
	}
}

