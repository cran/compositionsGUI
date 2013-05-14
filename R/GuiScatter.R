GuiScatter <-
function(window) {
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("Scatterplot")
	# is child of the main window
	w$setTransientFor(window)
	w$setDestroyWithParent(TRUE)
	# no reason to do this
	w$setResizable(FALSE)

	input <- guiVariableComboNew(guiVariableList(), guiGetData("ilrName"))
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

	col <- guiVariableComboNew(guiExternalsList())
	colBox <- gtkHBoxNew()
	colBox$packStart(guiPaddedLabelNew("colors as:"), FALSE, FALSE, 0)
	colBox$packStart(col, TRUE, TRUE, 0)

	pch <- guiVariableComboNew(guiExternalsList())
	pchBox <- gtkHBoxNew()
	pchBox$packStart(guiPaddedLabelNew("symbols as:"), FALSE, FALSE, 0)
	pchBox$packStart(pch, TRUE, TRUE, 0)

	spacebox <- gtkVBoxNew()
	spacebox$packStart(colBox, FALSE, FALSE, 0)
	spacebox$packStart(pchBox, FALSE, FALSE, 0)

	space <- gtkAlignmentNew()
	space$setPadding(10, 10, 0, 0)
	space$add(spacebox)

	helpButton = guiSetHelp("scatterplot", w, TRUE)

	# the 'load' button
	load <- gtkButtonNewWithLabel("Run")
	hbox <- gtkHBoxNew()
	hbox$packStart(helpButton, FALSE, FALSE, 0)
	hbox$packStart(load, TRUE, TRUE, 0)

	gSignalConnect(load, "clicked", function(d) {
		inputName <- input$getActiveText()
		if(nchar(inputName) == 0) return()

		cols = ""
		pchs = ""
		if(nchar(col$getActiveText())) cols = paste0(", col=as.integer(as.factor(", col$getActiveText(), "))")
		if(nchar(pch$getActiveText())) pchs = paste0(", pch=as.integer(as.factor(", pch$getActiveText(), "))")

		res <- try(guiEval("plot(as.data.frame(", inputName, ")", cols, pchs, ")"), TRUE)
		if(class(res) == "try-error") return(GuiError(w, paste("Cannot create scatterplot for", inputName), res))

		w$destroy()
	})

	vbox <- gtkVBoxNew()
	vbox$packStart(inputBox, FALSE, FALSE, 0)
	vbox$packStart(warningLabel, FALSE, FALSE, 0)
	vbox$packStart(space, FALSE, FALSE, 0)
	vbox$packStart(hbox, FALSE, FALSE, 0)
	w$add(vbox)

	# move that window into the center of main window
	guiWindowToCenter(window, w)
	w$show()
}

