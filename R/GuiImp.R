GuiImp <-
function(window) {
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("Imputation of missing values")
	# is child of the main window
	w$setTransientFor(window)
	w$setDestroyWithParent(TRUE)
	# no reason to do this
	w$setResizable(FALSE)

	helpButton = guiSetHelp("missings", w, TRUE)

	useName <-  guiGetData("impName")
	if(is.null(useName)) useName <- guiGetData("compName")

	input <- guiVariableComboNew(guiVariableList(), useName)
	inputBox <- gtkHBoxNew()
	inputBox$packStart(guiPaddedLabelNew("data set:"), FALSE, FALSE, 0)
	inputBox$packStart(input, TRUE, TRUE, 0)

	entry <- gtkEntryNew()
	entry$setSizeRequest(250, -1)
	entryBox <- gtkHBoxNew()
	entryBox$packStart(guiPaddedLabelNew("save to:"), FALSE, FALSE, 0)
	entryBox$packStart(entry, TRUE, TRUE, 0)

	changeFunction <- function(d) {
		entry$setText(paste0(input$getActiveText(), ".imp"))
	}
	changeFunction(NULL)
	gSignalConnect(input, "changed", changeFunction)

	# the 'load' button
	load <- gtkButtonNewWithLabel("Run")
	hbox <- gtkHBoxNew()
	hbox$packStart(helpButton, FALSE, FALSE, 0)
	hbox$packStart(load, TRUE, TRUE, 0)

	gSignalConnect(load, "clicked", function(d) {
		inputName <- input$getActiveText()
		impName <- entry$getText()
		if(nchar(inputName) == 0 || nchar(impName) == 0) return()

		res <- try(guiEval(impName, " <- ", "impAll(", inputName, ")"), TRUE)
		if(class(res) == "try-error") return(GuiError(w, paste("Cannot fill missings in data set", inputName), res))

		guiPrint(impName)

		guiSetData("impName", impName)
		guiCheckStatus()
		w$destroy()
	})

	# pack the window content to vertical box
	vbox <- gtkVBoxNew()
	vbox$packStart(inputBox, FALSE, FALSE, 0)
	vbox$packStart(entryBox, FALSE, FALSE, 0)

	vbox$packStart(hbox, FALSE, FALSE, 0)
	w$add(vbox)

	# move that window into the center of main window
	guiWindowToCenter(window, w)
	w$show()
}

