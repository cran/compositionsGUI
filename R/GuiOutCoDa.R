GuiOutCoDa <-
function(window) {
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("Outlier detection")
	# is child of the main window
	w$setTransientFor(window)
	w$setDestroyWithParent(TRUE)
	# no reason to do this
	w$setResizable(FALSE)

	helpButton = guiSetHelp("outliers", w, TRUE)

	useName <-  guiGetData("impName")
	if(is.null(useName)) useName <- guiGetData("compName")

	input <- guiVariableComboNew(guiVariableList(), useName)
	inputBox <- gtkHBoxNew()
	inputBox$packStart(guiPaddedLabelNew("data set:"), FALSE, FALSE, 0)
	inputBox$packStart(input, TRUE, TRUE, 0)

	entry <- gtkEntryNew()
	entryBox <- gtkHBoxNew()
	entryBox$packStart(guiPaddedLabelNew("save to:"), FALSE, FALSE, 0)

	changeFunction <- function(d) {
		entry$setText(paste0(input$getActiveText(), ".outer"))
	}
	changeFunction(NULL)
	gSignalConnect(input, "changed", changeFunction)

	entryBox$packStart(entry, TRUE, TRUE, 0)

	typePanel = gtkHBoxNew()
	robustBt = gtkRadioButtonNewWithLabel(NULL, "robust")
	standardBt = gtkRadioButtonNewWithLabel(robustBt$getGroup(), "classical")
	typePanel$packStart(guiPaddedLabelNew("method: "), FALSE, FALSE, 0);
	typePanel$packStart(robustBt, FALSE, FALSE, 0)
	typePanel$packStart(standardBt, FALSE, FALSE, 0)

	quantile <- gtkEntryNew()
	quantile$setText("0.975")
	quantileBox <- gtkHBoxNew()
	quantileBox$packStart(guiPaddedLabelNew("quantile:"), FALSE, FALSE, 0)
	quantileBox$packStart(quantile, TRUE, TRUE, 0)

	load <- gtkButtonNewWithLabel("Run")
	hbox <- gtkHBoxNew()
	hbox$packStart(helpButton, FALSE, FALSE, 0)
	hbox$packStart(load, TRUE, TRUE, 0)

	gSignalConnect(load, "clicked", function(d) {
		inputName <- input$getActiveText()
		outName <- entry$getText()
		if(nchar(inputName) == 0 || nchar(outName) == 0) return()

		method <- "robust"
		if(standardBt$getActive()) method <- "standard"
		quantileN <- try(as.numeric(quantile$getText()))
		if(is.na(quantileN) || quantileN <= 0 || quantileN >= 1) return(GuiError(w, paste("Invalid quantile:", quantile$getText())))

		res = try(guiEval(outName, " <- ", "outCoDa(", inputName, ", ", quantileN, ", ", guiS(method), ")"), TRUE)
		if(class(res) == "try-error") return(GuiError(w, paste("Cannot compute outliers", inputName), res))

		guiPrint(outName)
		guiPrint(outName, "$outlier")

		if(graphScatter$getActive() && graphDistance$getActive()) guiEval("par(mfrow=c(1, 2))")

		if(graphScatter$getActive()) {
			res = try(guiEval("plot(", outName, ")"), TRUE)
			if(class(res) == "try-error") return(GuiError(w, paste("Cannot plot outliers"), res))
		}
		if(graphDistance$getActive()) {
			res = try(guiEval("covPlot(isomLR(", inputName, "), which=\"dd\")"), TRUE)
			if(class(res) == "try-error") return(GuiError(w, paste("Cannot create distance plot"), res))
		}

		guiCheckStatus()
		w$destroy()
	})

	# pack the window content to vertical box
	space <- gtkAlignmentNew()
	space$setPadding(10, 10, 0, 0)
	spacebox <- gtkVBoxNew()
	space$add(spacebox)

	vbox <- gtkVBoxNew()
	vbox$packStart(inputBox, FALSE, FALSE, 0)
	vbox$packStart(entryBox, FALSE, FALSE, 0)

	spacebox$packStart(typePanel, FALSE, FALSE, 0)
	spacebox$packStart(quantileBox, FALSE, FALSE, 0)
	vbox$packStart(space, FALSE, FALSE, 0)

	graphScatter = gtkCheckButtonNewWithLabel("show scatter plot")
	graphDistance = gtkCheckButtonNewWithLabel("show distance-distance plot")

	alignScatter = gtkAlignmentNew(0, 0, 0, 0)
	alignScatter$setPadding(2, 10, 2, 0)
	labelScatter = gtkLabelNew("Robust Mahalanobis distances versus index of compositions")
	labelScatter$setLineWrap(TRUE)
	labelScatter$setSizeRequest(400, -1)
	alignScatter$add(labelScatter)

	alignDistance = gtkAlignmentNew(0, 0, 0, 0)
	alignDistance$setPadding(2, 10, 2, 0)
	labelDistance = gtkLabelNew("Displays both classical and robust Mahalanobis distances together with corresponding cut-off values")
	labelDistance$setLineWrap(TRUE)
	alignDistance$add(labelDistance)

	vbox$packStart(graphScatter, FALSE, FALSE, 0)
	vbox$packStart(alignScatter, FALSE, FALSE, 0)
	vbox$packStart(graphDistance, FALSE, FALSE, 0)
	vbox$packStart(alignDistance, FALSE, FALSE, 0)

	vbox$packStart(hbox, FALSE, FALSE, 0)
	w$add(vbox)

	# move that window into the center of main window
	guiWindowToCenter(window, w)
	w$show()
}

