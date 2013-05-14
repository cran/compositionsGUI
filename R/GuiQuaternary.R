GuiQuaternary <-
function(window) {
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("Quaternary diagram")
	# is child of the main window
	w$setTransientFor(window)
	w$setDestroyWithParent(TRUE)
	# no reason to do this
	w$setResizable(FALSE)

	useName <-  guiGetData("impName")
	if(is.null(useName)) useName <- guiGetData("compName")

	input <- guiVariableComboNew(guiVariableList(), useName)
	inputBox <- gtkHBoxNew()
	inputBox$packStart(guiPaddedLabelNew("data set:"), FALSE, FALSE, 0)
	inputBox$packStart(input, TRUE, TRUE, 0)

	col <- guiVariableComboNew(guiExternalsList())
	colBox <- gtkHBoxNew()
	colBox$packStart(guiPaddedLabelNew("colors as:"), FALSE, FALSE, 0)
	colBox$packStart(col, TRUE, TRUE, 0)

	spacebox <- gtkVBoxNew()
	spacebox$packStart(colBox, FALSE, FALSE, 0)

	space <- gtkAlignmentNew()
	space$setPadding(10, 10, 0, 0)
	space$add(spacebox)

	helpButton = guiSetHelp("quaternary", w, TRUE)

	# the 'load' button
	load <- gtkButtonNewWithLabel("Run")
	hbox <- gtkHBoxNew()
	hbox$packStart(helpButton, FALSE, FALSE, 0)
	hbox$packStart(load, TRUE, TRUE, 0)

	gSignalConnect(load, "clicked", function(d) {
		inputName <- input$getActiveText()
		if(nchar(inputName) == 0) return()

		cols = ""
		if(nchar(col$getActiveText())) cols = paste0(", col=as.integer(as.factor(", col$getActiveText(), "))")

		res = try(get(inputName), TRUE)
		if(class(res) != "try-error" && ncol(res) > 4) {
			dialog <- gtkMessageDialog(w, "destroy-with-parent", "warning", "close", "Only the first 4 variables will be used")
			dialog$run()
			dialog$destroy()
		}

		res <- try(guiEval("plot3D(acomp(", inputName, ")", cols, ")"), TRUE)
		if(class(res) == "try-error") return(GuiError(w, paste("Cannot plot quaternary diagram for", inputName), res))

		w$destroy()
	})

	warningLabel = guiPaddedLabelNew("Exactly 4 compositional variables necessary");

	vbox <- gtkVBoxNew()
	vbox$packStart(inputBox, FALSE, FALSE, 0)
	vbox$packStart(warningLabel, FALSE, FALSE, 0);
	vbox$packStart(space, FALSE, FALSE, 0);
	vbox$packStart(hbox, FALSE, FALSE, 0)
	w$add(vbox)

	# move that window into the center of main window
	guiWindowToCenter(window, w)
	w$show()
}

