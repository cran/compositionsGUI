GuiBoxplot <-
function(window) {
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("Boxplots of pairwise log-ratios")
	# is child of the main window
	w$setTransientFor(window)
	w$setDestroyWithParent(TRUE)
	# no reason to do this
	w$setResizable(FALSE)

	helpButton = guiSetHelp("boxplots", w, TRUE)

	useName <-  guiGetData("impName")
	if(is.null(useName)) useName <- guiGetData("compName")

	input <- guiVariableComboNew(guiVariableList(), useName)
	inputBox <- gtkHBoxNew()
	inputBox$packStart(guiPaddedLabelNew("data set:"), FALSE, FALSE, 0)
	inputBox$packStart(input, TRUE, TRUE, 0)

	cov <- guiVariableComboNew(guiExternalsList())
	covBox <- gtkHBoxNew()
	covBox$packStart(guiPaddedLabelNew("discrete covariate:"), FALSE, FALSE, 0)
	covBox$packStart(cov, TRUE, TRUE, 0)

	spacebox <- gtkVBoxNew()
	spacebox$packStart(covBox, FALSE, FALSE, 0)

	space <- gtkAlignmentNew()
	space$setPadding(10, 10, 0, 0)
	space$add(spacebox)

	# the 'load' button
	load <- gtkButtonNewWithLabel("Run")
	hbox <- gtkHBoxNew()
	hbox$packStart(helpButton, FALSE, FALSE, 0)
	hbox$packStart(load, TRUE, TRUE, 0)
	loadAlign <- gtkAlignmentNew()
	loadAlign$setPadding(5, 0, 0, 0)
	loadAlign$add(hbox)

	gSignalConnect(load, "clicked", function(d) {
		inputName <- input$getActiveText()
		if(nchar(inputName) == 0) return()

		covs = ""
		if(nchar(cov$getActiveText())) covs = paste0(", as.factor(", cov$getActiveText(), ")")

		res = try(guiEval("boxplot(acomp(", inputName, ")", covs, ")"), TRUE);
		if(class(res) == "try-error") return(GuiError(w, paste("Cannot create boxplots", inputName), res))

		guiCheckStatus()
		w$destroy()
	})

	# pack the window content to vertical box
	vbox <- gtkVBoxNew()
	vbox$packStart(inputBox, FALSE, FALSE, 0)
	vbox$packStart(space, FALSE, FALSE, 0)
	vbox$packStart(loadAlign, FALSE, FALSE, 0)
	w$add(vbox)

	# move that window into the center of main window
	guiWindowToCenter(window, w)
	w$show()
}

