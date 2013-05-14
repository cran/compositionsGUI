GuiPairwise <-
function(window) {
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("Scatterplots with covariates")
	# is child of the main window
	w$setTransientFor(window)
	w$setDestroyWithParent(TRUE)
	# no reason to do this
	w$setResizable(FALSE)

	input <- guiVariableComboNew(guiVariableList(), guiGetData("ilrName"))
	input$setSizeRequest(250, -1)
	transformButton = gtkButtonNewWithLabel("transform")
	inputBox <- gtkHBoxNew()
	inputBox$packStart(guiPaddedLabelNew("data set:"), FALSE, FALSE, 0)
	inputBox$packStart(input, TRUE, TRUE, 0)
	inputBox$packStart(transformButton, FALSE, FALSE, 0)

	warningLabel <- guiPaddedLabelNew("Input data set has to be already transformed using log-ratio transformations")

	gSignalConnect(transformButton, "clicked", function(d) {
		GuiIlr(w, input$getActiveText(), function() {
			active <- guiGetData("ilrName")

			for(i in 1:input$getModel()$iterNChildren()) input$removeText(0)
			for(item in guiVariableList()) {
				input$appendText(item)
				if(!is.null(active) && active == item) input$setActive(input$getModel()$iterNChildren()-1)
			}
		})
	})

	external <- guiVariableComboNew(guiVariableList(), guiGetData("externalName"))
	externalBox <- gtkHBoxNew()
	externalBox$packStart(guiPaddedLabelNew("covariates:"), FALSE, FALSE, 0)
	externalBox$packStart(external, TRUE, TRUE, 0)

	space <- gtkAlignmentNew()
	space$setPadding(10, 10, 0, 0)
	space$add(externalBox)

	lineButton = gtkCheckButtonNewWithLabel("add classical line")
	robustButton = gtkCheckButtonNewWithLabel("add robust line")

	helpButton = guiSetHelp("pairwiseplot", w, TRUE)

	# the 'load' button
	load <- gtkButtonNewWithLabel("Run")
	hbox <- gtkHBoxNew()
	hbox$packStart(helpButton, FALSE, FALSE, 0)
	hbox$packStart(load, TRUE, TRUE, 0)

	gSignalConnect(load, "clicked", function(d) {
		inputName <- input$getActiveText()
		externalName <- external$getActiveText()
		if(nchar(inputName) == 0 || nchar(externalName) == 0) return()

		res <- try(guiEval("pairwisePlot(", inputName, ", ", externalName, "[sapply(", externalName, ", function(x) is.numeric(x) || is.integer(x))], add.line=", lineButton$getActive(), ", line.col=2, add.robust=", robustButton$getActive(), ", rob.col=4)"), TRUE)
		if(class(res) == "try-error") return(GuiError(w, paste("Cannot create pairwise plot for", inputName), res))

		w$destroy()
	})

	vbox <- gtkVBoxNew()
	vbox$packStart(inputBox, FALSE, FALSE, 0)
	vbox$packStart(warningLabel, FALSE, FALSE, 0)
	vbox$packStart(space, FALSE, FALSE, 0)
	vbox$packStart(lineButton, FALSE, FALSE, 0)
	vbox$packStart(robustButton, FALSE, FALSE, 0)
	vbox$packStart(hbox, FALSE, FALSE, 0)
	w$add(vbox)

	# move that window into the center of main window
	guiWindowToCenter(window, w)
	w$show()
}

