GuiPwlr <-
function(window) {
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("Pairwise log-ratio plot with covariates")
	# is child of the main window
	w$setTransientFor(window)
	w$setDestroyWithParent(TRUE)
	# no reason to do this
	w$setResizable(FALSE)

	useName <-  guiGetData("impName")
	if(is.null(useName)) useName <- guiGetData("compName")

	input <- guiVariableComboNew(guiVariableList(), useName)
	input$setSizeRequest(250, -1)
	inputBox <- gtkHBoxNew()
	inputBox$packStart(guiPaddedLabelNew("data set:"), FALSE, FALSE, 0)
	inputBox$packStart(input, TRUE, TRUE, 0)

	cov <- guiVariableComboNew(guiExternalsList(TRUE), guiGetDefaultExternal(TRUE))
	covBox <- gtkHBoxNew()
	covBox$packStart(guiPaddedLabelNew("covariate:"), FALSE, FALSE, 0)
	covBox$packStart(cov, TRUE, TRUE, 0)

	lineButton = gtkCheckButtonNewWithLabel("add line")
	robustButton = gtkCheckButtonNewWithLabel("add robust line")
	helpButton = guiSetHelp("pairwiseplot", w, TRUE)

	xyPanel = gtkVBoxNew()
	xyBt = gtkRadioButtonNewWithLabel(NULL, "composition as X, covariate as Y")
	yxBt = gtkRadioButtonNewWithLabel(xyBt$getGroup(), "composition as Y, covariate as X")
	xyPanel$packStart(xyBt, FALSE, FALSE, 0)
	xyPanel$packStart(yxBt, FALSE, FALSE, 0)

	xyPanelPadding <- gtkAlignmentNew()
	xyPanelPadding$setPadding(10, 10, 0, 0)
	xyPanelPadding$add(xyPanel)

	# the 'load' button
	load <- gtkButtonNewWithLabel("Run")
	hbox <- gtkHBoxNew()
	hbox$packStart(helpButton, FALSE, FALSE, 0)
	hbox$packStart(load, TRUE, TRUE, 0)

	gSignalConnect(load, "clicked", function(d) {
		inputName <- input$getActiveText()
		covName <- cov$getActiveText()
		if(nchar(inputName) == 0 || nchar(covName) == 0) return()

		if(xyBt$getActive()) {
			first = paste0("acomp(", inputName, ")");
			second = covName
		} else {
			first = covName
			second = paste0("acomp(", inputName, ")");
		}

		res <- try(guiEval("pwlrPlot(", first, ", ", second, ", add.line=", lineButton$getActive(),", line.col=2, add.robust=", robustButton$getActive(), ", rob.col=4)"), TRUE)
		if(class(res) == "try-error") return(GuiError(w, paste("Cannot create pairwise plot for", inputName), res))

		w$destroy()
	})

	vbox <- gtkVBoxNew()
	vbox$packStart(inputBox, FALSE, FALSE, 0)
	vbox$packStart(covBox, FALSE, FALSE, 0)
	vbox$packStart(lineButton, FALSE, FALSE, 0)
	vbox$packStart(robustButton, FALSE, FALSE, 0)
	vbox$packStart(xyPanelPadding, FALSE, FALSE, 0)
	vbox$packStart(hbox, FALSE, FALSE, 0)
	w$add(vbox)

	# move that window into the center of main window
	guiWindowToCenter(window, w)
	w$show()
}

