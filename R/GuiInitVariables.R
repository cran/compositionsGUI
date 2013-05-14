GuiInitVariables <-
function(window, originalName = guiGetData("originalName")) {
	# get the data set and column names
	if(is.null(originalName)) return(GuiError(window, "No original data set name found"))
	d <- try(get(originalName, envir=.GlobalEnv), TRUE)
	if(class(d) == "try-error") return(GuiError(window, paste("Cannot load variable", originalName)))
	cnames = colnames(d)

	# child of the main window
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("Variable selection")
	w$setTransientFor(window)
	w$setDestroyWithParent(TRUE)

	guiSetHelp("selectcolumnscolumns", w)

	leftListStore = gtkListStoreNew("gboolean", "gchararray");
	rightListStore = gtkListStoreNew("gboolean", "gchararray");

	leftList = guiInitColumnsTreeView(cnames, TRUE, guiGetData("compIndexes"), leftListStore, rightListStore)
	rightList = guiInitColumnsTreeView(cnames, FALSE, guiGetData("externalIndexes"), rightListStore, leftListStore)

	leftbox = gtkVBoxNew()
	leftbox$packStart(gtkLabelNew("compositional parts"), FALSE, FALSE)
	leftnamebox = gtkHBoxNew()
	leftnamebox$packStart(gtkLabelNew("name:"), FALSE, FALSE)
	leftname = gtkEntryNew()
	if(is.null(guiGetData("compName"))) leftname$setText("comp")
	else leftname$setText(guiGetData("compName"))
	leftnamebox$packStart(leftname, TRUE, TRUE)
	leftbox$packStart(leftnamebox, FALSE, FALSE)
	leftboxscroll = gtkScrolledWindowNew()
	leftboxscroll$setPolicy("automatic", "automatic")
	leftboxscroll$add(leftList)
	leftbox$packStart(leftboxscroll, TRUE, TRUE)

	leftinvertbox = gtkHBoxNew()
	leftinvert = gtkButtonNew()
	leftinvert$setLabel("Invert")
	leftinvertbox$packStart(leftinvert, FALSE, FALSE)
	leftbox$packStart(leftinvertbox, FALSE, FALSE)

	rightbox = gtkVBoxNew()
	rightbox$packStart(gtkLabelNew("external variables"), FALSE, FALSE)
	rightnamebox = gtkHBoxNew()
	rightnamebox$packStart(gtkLabelNew("name:"), FALSE, FALSE)
	rightname = gtkEntryNew()
	if(is.null(guiGetData("externalName"))) rightname$setText("external")
	else rightname$setText(guiGetData("externalName"))
	rightnamebox$packStart(rightname, TRUE, TRUE)
	rightbox$packStart(rightnamebox, FALSE, FALSE)
	rightboxscroll = gtkScrolledWindowNew()
	rightboxscroll$setPolicy("automatic", "automatic")
	rightboxscroll$add(rightList)
	rightbox$packStart(rightboxscroll, TRUE, TRUE)

	rightinvertbox = gtkHBoxNew()
	rightinvert = gtkButtonNew()
	rightinvert$setLabel("Invert")
	rightinvertbox$packStart(rightinvert, FALSE, FALSE)
	rightbox$packStart(rightinvertbox, FALSE, FALSE)

	hbox <- gtkHBoxNew()
	hbox$packStart(leftbox, TRUE, TRUE, 3)
	hbox$packStart(rightbox, TRUE, TRUE, 3)

	naRepresentation <- guiComboNew(c("0", "negative values", "NA", "other"))
	naRepresentation$setActive(2)
	naRepresentationOther <- gtkEntryNew()
	naRepresentationOther$setSensitive(FALSE)
	gSignalConnect(naRepresentation, "changed", function(obj) {
		naRepresentationOther$setSensitive(naRepresentation$getActive() == 3)
	})

	zeroRepresentation <- guiComboNew(c("0", "negative values", "NA", "other"))
	zeroRepresentation$setActive(1)
	zeroRepresentationOther <- gtkEntryNew()
	zeroRepresentationOther$setSensitive(FALSE)
	gSignalConnect(zeroRepresentation, "changed", function(obj) {
		zeroRepresentationOther$setSensitive(zeroRepresentation$getActive() == 3)
	})

        # first create the HBox, then pack it into the window's VBox (windowBox)
        naBox <- gtkHBoxNew()
        naBox$packStart(guiPaddedLabelNew("Coding for missing values:"), FALSE, FALSE, 0)
        naBox$packStart(naRepresentation, TRUE, TRUE, 0)
        naBox$packStart(naRepresentationOther, FALSE, FALSE, 0)

	naBoxAlign <- gtkAlignmentNew()
	naBoxAlign$add(naBox)
	naBoxAlign$setPadding(10, 0, 0, 0)

        zeroBox <- gtkHBoxNew()
        zeroBox$packStart(guiPaddedLabelNew("Coding for rounded zero:"), FALSE, FALSE, 0)
        zeroBox$packStart(zeroRepresentation, TRUE, TRUE, 0)
        zeroBox$packStart(zeroRepresentationOther, FALSE, FALSE, 0)

	zeroBoxAlign <- gtkAlignmentNew()
	zeroBoxAlign$add(zeroBox)
	zeroBoxAlign$setPadding(0, 10, 0, 0)

	allbox  <- gtkVBoxNew()
	allbox$packStart(hbox, TRUE, TRUE)
	allbox$packStart(naBoxAlign, FALSE, FALSE)
	allbox$packStart(zeroBoxAlign, FALSE, FALSE)
	okbuttonbox = gtkHBoxNew()
	okbutton = gtkButtonNew()
	okbuttonbox$packEnd(okbutton, FALSE, FALSE, 3)
	okbutton$setLabel("OK")
	okbutton$setSizeRequest(60,30)
	allbox$packStart(okbuttonbox, FALSE, FALSE, 3)

	# left invert
	gSignalConnect(leftinvert, "clicked", function(obj) {
		guiInvertToggleList(leftList$getModel(), rightList$getModel())
	})
	# right invert
	gSignalConnect(rightinvert, "clicked", function(obj) {
		guiInvertToggleList(rightList$getModel(), leftList$getModel())
	})
	gSignalConnect(okbutton, "clicked", function(obj) {
		if(naRepresentation$getActive() == zeroRepresentation$getActive() && (naRepresentation$getActive() != 3 || naRepresentationOther$getText() == zeroRepresentationOther$getText())) return(GuiError(w, "NA and zero representations cannot be the same"))

		if(class(get(originalName)) != "data.frame") guiEval(originalName, " <- as.data.frame(", originalName, ")")

		indexNA <- naRepresentation$getActive()
		indexZero <- zeroRepresentation$getActive()
		if(indexNA == 0 || indexNA == 1 || indexNA == 3 || indexZero == 2 || indexZero == 3) {
			tempName = ".comp"
			guiEval(tempName, " <- ", originalName)

			# do replacements of NAs and zeroes if necessary
			if(indexNA == 0) guiEval(originalName, "[", tempName, "==0] = NA")
			else if(indexNA == 1) guiEval(originalName, "[", tempName, "<0] = NA")
			else if(indexNA == 3) guiEval(originalName, "[", tempName, " == ", guiS(naRepresentationOther$getText()), "] = NA")

			if(indexZero == 2) guiEval(originalName, "[is.na(", tempName, ")] = 0")
			else if(indexZero == 3) guiEval(originalName, "[", tempName, " == ", guiS(zeroRepresentationOther$getText()), "] = 0")
		}
		dlNegative <- (indexZero==1)

		# data from the window
		compIndexes = guiActivatedToggleList(leftList$getModel())
		externalIndexes = guiActivatedToggleList(rightList$getModel())
		compName = leftname$getText()
		externalName = rightname$getText()

		# any problem? just show error and leave this handler
		if(length(compIndexes) < 2) return(GuiError(w, "At least 2 compositional columns have to be selected"))
		if(nchar(compName) < 1) return(GuiError(w, "Specify variable name for the compositional part"))
		if(nchar(externalName) < 1) return(GuiError(w, "Specify variable name for the external part"))

		# validate if everything is numeric
		res = try(guiCheckData(get(originalName, envir=.GlobalEnv)[,compIndexes], TRUE), TRUE)
		if(class(res) == "try-error") return(GuiError(w, "Compositional parts error", res))

		# assign the data set to specified variables
		res = try(guiEval(compName, " <- ", originalName, "[,c(", paste(compIndexes, collapse=", "), ")]"), TRUE)
		if(class(res) == "try-error") return(GuiError(w, c("Cannot write to ", compName), res))

		res = try(guiEval(externalName, " <- ", originalName, "[,c(", paste(externalIndexes, collapse=", "), ")]"), TRUE)
		if(class(res) == "try-error") return(GuiError(w, c("Cannot write to ", externalName), res))

		d <- get(compName, envir=.GlobalEnv)

		naimpute = FALSE
		zeroimpute = FALSE
		if(any(is.na(d))) naimpute = TRUE
		if(dlNegative && any(na.omit(d) < 0)) zeroimpute = TRUE
		if(!dlNegative && any(na.omit(d) < 0)) {
			dialog <- gtkMessageDialog(window, "destroy-with-parent", "error", "close", "Data set contains negative values, but no detection selected")
			dialog$run()
			dialog$destroy()
			return()
		}
		if(!any(na.omit(d) < 0) && any(na.omit(d) == 0)) {
			dialog <- gtkMessageDialog(window, "destroy-with-parent", "error", "close", "Data set contains zeroes, but no detection limit supplied. Cannot impute values below detection limit.")
			dialog$run()
			dialog$destroy()
		}

		# save the data sets to global
		guiSetData("compIndexes", compIndexes)
		guiSetData("externalIndexes", externalIndexes)
		guiSetData("compName", compName)
		guiSetData("externalName", externalName)
		guiSetData("originalName", originalName)
		guiSetData("helpMain", "dataloaded")
		guiSetHelp("init") # forces rewrite of the message, but no new bind to the window

		guiCheckStatus()

		# and go back to main window
		w$destroy()

		guiPrint(compName)
		guiPrint("print(Patterns(", compName, "))")

		error = NULL
		if(naimpute && zeroimpute) error = "Warning: data set contains missing values and values below detection limit"
		else if(naimpute) error = "Warning: data set contains missing values"
		else if(zeroimpute) error = "Warning: data set contains values below detection limit"

		if(!is.null(error)) {
			dialog <- gtkMessageDialog(window, "destroy-with-parent", "question", "yes-no", paste0(error, ", do you want to try to impute them?"))
			response <- dialog$run()
			dialog$destroy()
			if(response == -8) GuiImp(window)
		}
	})

	# some minimum size, and show
	allbox$setSizeRequest(400, 400)
	w$add(allbox)
	w$show()
}

