GuiSummary <-
function(window) {
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("Summary statistics")
	# is child of the main window
	w$setTransientFor(window)
	w$setDestroyWithParent(TRUE)
	# no reason to do this
	w$setResizable(FALSE)

	helpButton = guiSetHelp("summary", w, TRUE)

	useName <-  guiGetData("impName")
	if(is.null(useName)) useName <- guiGetData("compName")

	input <- guiVariableComboNew(guiVariableList(), useName)
	inputBox <- gtkHBoxNew()
	inputBox$packStart(guiPaddedLabelNew("data set:"), FALSE, FALSE, 0)
	inputBox$packStart(input, TRUE, TRUE, 0)

	entry <- gtkEntryNew()
	entryBox <- gtkHBoxNew()
	entryBox$packStart(guiPaddedLabelNew("save to:"), FALSE, FALSE, 0)
	entryBox$packStart(entry, TRUE, TRUE, 0)

	changeFunction <- function(d) {
		entry$setText(paste0(input$getActiveText(), ".summary"))
	}
	changeFunction(NULL)
	gSignalConnect(input, "changed", changeFunction)

	typePanel = gtkHBoxNew()
	robustBt = gtkRadioButtonNewWithLabel(NULL, "robust")
	standardBt = gtkRadioButtonNewWithLabel(robustBt$getGroup(), "classical")
	typePanel$packStart(guiPaddedLabelNew("method: "), FALSE, FALSE, 0);
	typePanel$packStart(robustBt, FALSE, FALSE, 0)
	typePanel$packStart(standardBt, FALSE, FALSE, 0)

	checksPanel = gtkVBoxNew()

	checkTitleAlign = gtkAlignmentNew(0,0)
	checkTitle = gtkLabelNew("select statistics:")
	gtkMiscSetAlignment(checkTitle, 0, 0)
	gtkMiscSetPadding(checkTitle, 5, 3)
	checkTitleAlign$add(checkTitle)
	checksPanel$packStart(checkTitleAlign, FALSE, FALSE, 0)

	meanButton = gtkCheckButtonNewWithLabel("center")
	variationButton = gtkCheckButtonNewWithLabel("variation matrix")
	expButton = gtkCheckButtonNewWithLabel("expsd")
	minButton = gtkCheckButtonNewWithLabel("min")
	q1Button = gtkCheckButtonNewWithLabel("first quartile")
	medButton = gtkCheckButtonNewWithLabel("median")
	q3Button = gtkCheckButtonNewWithLabel("third quartile")
	maxButton = gtkCheckButtonNewWithLabel("max")

	checksPanel$packStart(meanButton, FALSE, FALSE, 0)
	checksPanel$packStart(variationButton, FALSE, FALSE, 0)
	checksPanel$packStart(expButton, FALSE, FALSE, 0)
	checksPanel$packStart(minButton, FALSE, FALSE, 0)
	checksPanel$packStart(q1Button, FALSE, FALSE, 0)
	checksPanel$packStart(medButton, FALSE, FALSE, 0)
	checksPanel$packStart(q3Button, FALSE, FALSE, 0)
	checksPanel$packStart(maxButton, FALSE, FALSE, 0)

	load <- gtkButtonNewWithLabel("Run")
	hbox <- gtkHBoxNew()
	hbox$packStart(helpButton, FALSE, FALSE, 0)
	hbox$packStart(load, TRUE, TRUE, 0)

	gSignalConnect(load, "clicked", function(d) {
		inputName <- input$getActiveText()
		outName <- entry$getText()
		if(nchar(inputName) == 0 || nchar(outName) == 0) return()

		method <- TRUE
		if(standardBt$getActive()) method <- FALSE

		res <- try(guiEval(outName, " <- ", "summary(acomp(", inputName, "), robust=", method, ")"), TRUE)
		if(class(res) == "try-error") return(GuiError(w, paste("Cannot compute summary", inputName), res))

		toinclude = c()

		if(meanButton$getActive()) toinclude = c(toinclude, "mean")
		if(variationButton$getActive()) toinclude = c(toinclude, "variation")
		if(expButton$getActive()) toinclude = c(toinclude, "expsd")
		if(minButton$getActive()) toinclude = c(toinclude, "min")
		if(q1Button$getActive()) toinclude = c(toinclude, "q1")
		if(medButton$getActive()) toinclude = c(toinclude, "med")
		if(q3Button$getActive()) toinclude = c(toinclude, "q3")
		if(maxButton$getActive()) toinclude = c(toinclude, "max")

		if(length(toinclude) > 0) guiPrint(outName, "[", toinclude, "]")

		w$destroy()
	})

	all <- gtkVBoxNew()
	all$packStart(inputBox, FALSE, FALSE, 0)
	all$packStart(entryBox, FALSE, FALSE, 0)
	all$packStart(typePanel, FALSE, FALSE, 0)
	all$packStart(checksPanel, FALSE, FALSE, 0)
	all$packStart(hbox, FALSE, FALSE, 0)

	w$add(all)
	# move that window into the center of main window
	guiWindowToCenter(window, w)
	w$show()
}

