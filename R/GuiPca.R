GuiPca <-
function(window) {
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("Principal component analysis")
	# is child of the main window
	w$setTransientFor(window)
	w$setDestroyWithParent(TRUE)
	# no reason to do this
	w$setResizable(FALSE)

	helpButton = guiSetHelp("pca", w, TRUE)

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
		entry$setText(paste0(input$getActiveText(), ".pc"))
	}
	changeFunction(NULL)
	gSignalConnect(input, "changed", changeFunction)

	typePanel = gtkHBoxNew()
	robustBt = gtkRadioButtonNewWithLabel(NULL, "robust")
	standardBt = gtkRadioButtonNewWithLabel(robustBt$getGroup(), "classical")
	typePanel$packStart(guiPaddedLabelNew("method: "), FALSE, FALSE, 0);
	typePanel$packStart(robustBt, FALSE, FALSE, 0)
	typePanel$packStart(standardBt, FALSE, FALSE, 0)

	graphTypePanel = gtkHBoxNew()
	loadingsButton = gtkCheckButtonNewWithLabel("show loadings")
	scoresButton = gtkCheckButtonNewWithLabel("show scores")
	biplotButton = gtkCheckButtonNewWithLabel("show biplot")
	screeButton = gtkCheckButtonNewWithLabel("show scree plot")

	load <- gtkButtonNewWithLabel("Run")
	load <- gtkButtonNewWithLabel("Run")
	hbox <- gtkHBoxNew()
	hbox$packStart(helpButton, FALSE, FALSE, 0)
	hbox$packStart(load, TRUE, TRUE, 0)

	gSignalConnect(load, "clicked", function(d) {
		inputName <- input$getActiveText()
		pcaName <- entry$getText()
		if(nchar(inputName) == 0 || nchar(pcaName) == 0) return()

		method <- "robust"
		if(standardBt$getActive()) method <- "standard"

		res = try(guiEval(pcaName, " <- pcaCoDa(", inputName, ", method=", guiS(method), ")"), TRUE)
		if(class(res) == "try-error") return(GuiError(w, paste("Cannot compute PCA for", inputName), res))

		guiPrint(pcaName)
		if(loadingsButton$getActive()) guiPrint(pcaName, "$loadings")
		if(scoresButton$getActive()) guiPrint(pcaName, "$scores")

		if(biplotButton$getActive() && screeButton$getActive()) guiEval("par(mfrow=c(1, 2))")

		if(biplotButton$getActive()) {
			res = try(guiEval("plot(", pcaName, ")"), TRUE)
			if(class(res) == "try-error") return(GuiError(w, paste("Cannot create biplot"), res))
		}
		if(screeButton$getActive()) {
			if(standardBt$getActive()) {
				res = try(guiEval("screeplot(princomp(isomLR(", inputName, ")), type=\"lines\", main=\"Scree plot of PCA eigenvalues\")"), TRUE)
			} else {
				res = try(guiEval("screeplot(princomp(covmat=covMcd(isomLR(", useName, "))$cov), type=\"lines\", main=\"Scree plot of PCA eigenvalues\")"), TRUE)
			}
			
			if(class(res) == "try-error") return(GuiError(w, paste("Cannot create screeplot"), res))
			res = try(guiEval("grid(nx=NULL, ny=NULL, lty=2, lwd=1, col=\"gray60\", equilogs=TRUE)"), TRUE)
			if(class(res) == "try-error") return(GuiError(w, paste("Cannot plot grid"), res))
		}

		w$destroy()
	})

	all <- gtkVBoxNew()
	all$packStart(inputBox, FALSE, FALSE, 0)
	all$packStart(entryBox, FALSE, FALSE, 0)
	all$packStart(typePanel, FALSE, FALSE, 0)
	all$packStart(loadingsButton, FALSE, FALSE, 0)
	all$packStart(scoresButton, FALSE, FALSE, 0)
	all$packStart(biplotButton, FALSE, FALSE, 0)
	all$packStart(screeButton, FALSE, FALSE, 0)
	all$packStart(hbox, FALSE, FALSE, 0)

	w$add(all)
	# move that window into the center of main window
	guiWindowToCenter(window, w)
	w$show()
}

