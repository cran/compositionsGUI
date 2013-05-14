GuiOutClass <- 
function(window) {
	useName <-  guiGetData("impName")
	if(is.null(useName)) useName <- guiGetData("compName")
	
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("Outlier classification")
	# is child of the main window
	w$setTransientFor(window)
	w$setDestroyWithParent(TRUE)
	# no reason to do this
	w$setResizable(FALSE)

	helpButton = guiSetHelp("outlierclassifier", w, TRUE)

	input <- guiVariableComboNew(guiVariableList(), useName)
	inputBox <- gtkHBoxNew()
	inputBox$packStart(guiPaddedLabelNew("input:"), FALSE, FALSE, 0)
	inputBox$packStart(input, TRUE, TRUE, 0)

	entry <- gtkEntryNew()
	# first create the HBox, then pack it into the window's VBox (windowBox)
	entryBox <- gtkHBoxNew()
	entryBox$packStart(guiPaddedLabelNew("save to:"), FALSE, FALSE, 0)
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

	graphTernary = gtkCheckButtonNewWithLabel("")
	alignTernary = gtkAlignmentNew(0, 0, 0, 0)
	alignTernary$setPadding(2, 10, 2, 0)
	labelTernary = gtkLabelNew("")
	labelTernary$setLineWrap(TRUE)
	alignTernary$add(labelTernary)

	changeFunction <- function(d) {
		entry$setText(paste0(input$getActiveText(), ".classifier"))

		numcol = try(ncol(get(input$getActiveText(), envir=.GlobalEnv)), TRUE)
		load$setSensitive(class(numcol) != "try-error" && numcol >= 3)

		if(class(numcol) == "try-error" || numcol < 3) {
			graphTernary$setLabel("")
			labelTernary$setLabel("Invalid input data")
		} else if(numcol == 3) {
			graphTernary$setLabel("show ternary diagram")
			labelTernary$setLabel("Displays ternary diagram of regular observations, extreme values and outliers")
		} else {
			graphTernary$setLabel("show biplot")
			labelTernary$setLabel("Displays biplot of regular observations, extreme values and outliers")
		}
	}
	changeFunction(NULL)
	gSignalConnect(input, "changed", changeFunction)

	gSignalConnect(load, "clicked", function(d) {
		inputName <- input$getActiveText()
		outName <- entry$getText()
		if(nchar(inputName) == 0 || nchar(outName) == 0) return()

		data <- get(inputName, envir=.GlobalEnv)
		if(ncol(data) < 3) return(GuiError(window, "Too few variables in the data set"))

		quantileN <- try(as.numeric(quantile$getText()))
		if(is.na(quantileN) || quantileN <= 0 || quantileN >= 1) return(GuiError(w, paste("Invalid quantile:", quantile$getText())))

		dialog <- gtkMessageDialog(window, "destroy-with-parent", "question", "yes-no", "This computation is very slow, especially for large number of variables. Do you really want to continue?")
		response <- dialog$run()
		dialog$destroy()
		if(response != -8) return()

		res = try(guiEval(outName, " <- OutlierClassifier1(acomp(", inputName, "), type=\"grade\", robust=", robustBt$getActive(), ", alpha=", 1-quantileN, ")"), TRUE)
		if(class(res) == "try-error") return(GuiError(w, "Cannot compute outlier classifier", res))
		classifier <- res

		guiPrint(outName)
		guiEval(".classgraph = rep(0, length(", outName, "))")
		guiEval(".classgraph[", outName, " == \"ok\"] <- 1")
		guiEval(".classgraph[", outName, " == \"grade\"] <- 2")
		guiEval(".classgraph[", outName, " == \"extreme\"] <- 3")

		if(graphTernary$getActive()) {
			if(ncol(data) == 3) {
				res = try(guiEval("ternaryDiag(", inputName, ", col=.classgraph, pch=.classgraph)"), TRUE)
				if(class(res) == "try-error") return(GuiError(w, "Cannot plot ternary diagram", res))
			} else if(ncol(data) > 3) {
				res = try(guiEval("coloredBiplot(princomp(", inputName, "), xcol=.classgraph, xpch=.classgraph, xnames=rep(\"\", times=nrow(", inputName, ")))"), TRUE)
				if(class(res) == "try-error") return(GuiError(w, "Cannot show biplot", res))
			}
		}

		guiCheckStatus()
		w$destroy()
	})

	vbox$packStart(graphTernary, FALSE, FALSE, 0)
	vbox$packStart(alignTernary, FALSE, FALSE, 0)

	vbox$packStart(hbox, FALSE, FALSE, 0)
	w$add(vbox)

	# move that window into the center of main window
	guiWindowToCenter(window, w)
	w$show()
}

