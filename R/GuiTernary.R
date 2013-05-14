GuiTernary <-
function(window) {
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("Ternary diagram")
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

	helpButton = guiSetHelp("ternary", w, TRUE)

	typePanel = gtkHBoxNew()
	robustBt = gtkRadioButtonNewWithLabel(NULL, "robust")
	standardBt = gtkRadioButtonNewWithLabel(robustBt$getGroup(), "classical")
	typePanel$packStart(guiPaddedLabelNew("method: "), FALSE, FALSE, 0);
	typePanel$packStart(robustBt, FALSE, FALSE, 0)
	typePanel$packStart(standardBt, FALSE, FALSE, 0)

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

		method <- TRUE
		if(standardBt$getActive()) method <- FALSE
		plotMissings <- missingsButton$getActive()
		pca  <-  pcaButton$getActive()

		res <- try(guiEval("plot(acomp(", inputName, "), pca=", pca, ", plotMissings=", plotMissings, ", robust=", method, cols, pchs, ")"), TRUE)
		if(class(res) == "try-error") return(GuiError(w, paste("Cannot plot ternary diagram for", inputName), res))

		if(isoPortionButton$getActive()) {
			guiEval("isoPortionLines()")
		}
		if(ellipseButton$getActive()) {
			qq = as.numeric(quantile$getText())

			res <- try(guiEval("ellipses(mean=mean(acomp(", inputName, "), robust=", method, "), var=var(acomp(", inputName, "), robust=", method, "), r=sqrt(qf(", qq, ", 2, nrow(", inputName, ")-2)*(2/(nrow(", inputName, ")-2))), col=\"red\")"), TRUE);
			if(class(res) == "try-error") return(GuiError(w, paste("Cannot plot ellipse for", inputName), res))
		}
		
		w$destroy()
	})

	isoPortionButton = gtkCheckButtonNewWithLabel("add iso portion lines")
	pcaButton = gtkCheckButtonNewWithLabel("first principal component")
	missingsButton = gtkCheckButtonNewWithLabel("show missings")
	ellipseButton = gtkCheckButtonNewWithLabel("add confidence ellipse for center")

	quantile <- gtkEntryNew()
	quantile$setText("0.95")
	quantile$setSensitive(FALSE)
	quantileBox <- gtkHBoxNew()
	quantileLabel <- guiPaddedLabelNew("confidence level:")
	quantileLabel$setSensitive(FALSE)
	quantileBox$packStart(quantileLabel, FALSE, FALSE, 0)
	quantileBox$packStart(quantile, TRUE, TRUE, 0)

	gSignalConnect(ellipseButton, "toggled", function(d) {
		quantile$setSensitive(ellipseButton$getActive())
		quantileLabel$setSensitive(ellipseButton$getActive())
	})

	warningLabel = guiPaddedLabelNew("");

	changeFunction <- function(d) {
		d = try(get(input$getActiveText(), envir=.GlobalEnv), TRUE)
		if(class(d) == "try-error") numcol = 0
		else numcol = ncol(d)

		isoPortionButton$setSensitive(numcol == 3)
		isoPortionButton$setActive(numcol == 3)

		sumMiss = if(numcol == 3) sum(Patterns(d)$missingStructure$rsum) else 0
		missingsButton$setSensitive(numcol == 3 && sumMiss > 0)
		missingsButton$setActive(numcol == 3 && sumMiss > 0)

		load$setSensitive(numcol >= 3)

		if(numcol > 3) {
			warningLabel$setText("Warning: in case of more than three parts multidimensional diagram will be plotted")
		} else if(numcol < 3) {
			warningLabel$setText("At least three parts necessary for ternary diagram")
		}
	}
	changeFunction(NULL)
	gSignalConnect(input, "changed", changeFunction)

	vbox <- gtkVBoxNew()
	vbox$packStart(inputBox, FALSE, FALSE, 0)
	vbox$packStart(typePanel, FALSE, FALSE, 0)
	vbox$packStart(warningLabel, FALSE, FALSE, 0);
	vbox$packStart(isoPortionButton, FALSE, FALSE, 0)
	vbox$packStart(pcaButton, FALSE, FALSE, 0)
	vbox$packStart(missingsButton, FALSE, FALSE, 0)
	vbox$packStart(ellipseButton, FALSE, FALSE, 0)
	vbox$packStart(quantileBox, FALSE, FALSE, 0)
	vbox$packStart(space, FALSE, FALSE, 0)
	vbox$packStart(hbox, FALSE, FALSE, 0)
	w$add(vbox)

	# move that window into the center of main window
	guiWindowToCenter(window, w)
	w$show()
}

