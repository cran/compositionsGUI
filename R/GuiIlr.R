GuiIlr <-
function(window, default = NULL, callback = NULL) {
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("LR transformation")
	# is child of the main window
	w$setTransientFor(window)
	w$setDestroyWithParent(TRUE)
	# no reason to do this
	w$setResizable(FALSE)

	activeMethod = "ilr"

	helpButton <- guiSetHelp(activeMethod, w, TRUE, function() {
		return(activeMethod)
	})

	if(!is.null(default) && nchar(default) > 0) useName <- default
	else useName <-  guiGetData("impName")
	if(is.null(useName)) useName <- guiGetData("compName")

	input <- guiVariableComboNew(guiVariableList(), useName)
	inputBox <- gtkHBoxNew()
	inputBox$packStart(guiPaddedLabelNew("input:"), FALSE, FALSE, 0)
	inputBox$packStart(input, TRUE, TRUE, 0)

	entry <- gtkEntryNew()
	entry$setSizeRequest(250, -1)
	entryBox <- gtkHBoxNew()
	entryBox$packStart(guiPaddedLabelNew("save to:"), FALSE, FALSE, 0)
	entryBox$packStart(entry, TRUE, TRUE, 0)
	entryBoxAlign <- gtkAlignmentNew()
	entryBoxAlign$setPadding(0, 5, 0, 0)
	entryBoxAlign$add(entryBox)

	type <- guiComboNew(c("isometric", "additive", "centered", "balances"))
	type$setActive(0)

        # first create the HBox, then pack it into the window's VBox (windowBox)
        typeBox <- gtkHBoxNew()
        typeBox$packStart(guiPaddedLabelNew("type:"), FALSE, FALSE, 0)
        typeBox$packStart(type, TRUE, TRUE, 0)

	typeBoxAlign <- gtkAlignmentNew()
	typeBoxAlign$setPadding(0, 10, 0, 0)
	typeBoxAlign$add(typeBox)

	ncols = ncol(get(useName, envir=.GlobalEnv))

	listnames = c()
	rationing <- gtkComboBoxNewText()
	rationingBox <- gtkHBoxNew()
	rationingBox$setVisible(FALSE)
	rationingBox$packStart(guiPaddedLabelNew("rationing part:"), FALSE, FALSE, 0)
	rationingBox$packStart(rationing, TRUE, TRUE, 0)

	balances <- gtkTextViewNew()
	balances$setWrapMode(1)
	balances$setSizeRequest(400,100)

	balancesFrame <- gtkFrameNew()
	balancesFrame$add(balances)

	baltext = "~"
	balnames = colnames(get(useName, envir=.GlobalEnv))
	while((ll=length(balnames)) > 2) {
		balnames = c(balnames[1:(ll-2)], paste0("(", balnames[ll-1], "/", balnames[ll], ")"))
	}
	baltext = paste(balnames, collapse="/")	
	balances$getBuffer()$setText(paste0("~", baltext))

	balancesAlign <- gtkAlignmentNew()
	balancesAlign$setVisible(FALSE)
	balancesAlign$setPadding(5, 0, 5, 5)
	balancesAlign$add(balancesFrame)

	load <- gtkButtonNewWithLabel("Run")
	hbox <- gtkHBoxNew()
	hbox$packStart(helpButton, FALSE, FALSE, 0)
	hbox$packStart(load, TRUE, TRUE, 0)
	loadAlign <- gtkAlignmentNew()
	loadAlign$setPadding(5, 0, 0, 0)
	loadAlign$add(hbox)

	changeFunction1 <- function(d) {
		names = c("ilr", "alr", "clr", "balances")
		activeMethod <<- names[[type$getActive()+1]]
		entry$setText(paste0(input$getActiveText(), ".", activeMethod))

		index <- type$getActive()
		rationingBox$setVisible(index == 1)
		balancesAlign$setVisible(index == 3)

		guiSetHelp(activeMethod, w, FALSE, TRUE)
	}
	changeFunction2 <- function(d) {
		res = try(get(input$getActiveText(), envir=.GlobalEnv), TRUE)
		if(class(res) != "try-error") {
			for(name in listnames) rationing$removeText(0)
			listnames <<- colnames(res)
			for(name in listnames) rationing$appendText(name)
			if(length(listnames) >= 1) rationing$setActive(length(listnames) - 1)
		}
	}
	changeFunction1(NULL)
	changeFunction2(NULL)
	gSignalConnect(type, "changed", changeFunction1)
	gSignalConnect(input, "changed", changeFunction2)

	gSignalConnect(load, "clicked", function(d) {
		inputName <- input$getActiveText()
		ilrName <- entry$getText()
		if(nchar(inputName) == 0 || nchar(ilrName) == 0) return()

		index = type$getActive()
		if(index == 0) {
			res <- try(guiEval(ilrName, " <- ", "isomLR(", inputName, ")"), TRUE)
			if(class(res) == "try-error") return(GuiError(w, paste("Cannot do isomLR with data set", inputName), res))
		} else if(index == 1) {
			res <- try(guiEval(ilrName, " <- ", "addLR(", inputName, ", ivar=", rationing$getActive()+1, ")$x.alr"), TRUE)
			if(class(res) == "try-error") return(GuiError(w, paste("Cannot do addLR with data set", inputName), res))
		} else if(index == 2) {
			res  <- try(guiEval(ilrName, " <- ", "cenLR(", inputName, ")$x.clr"), TRUE)
			if(class(res) == "try-error") return(GuiError(w, paste("Cannot do cenLR with data set", inputName), res))
		} else {
			t <- balances$getBuffer()$getText(balances$getBuffer()$getStartIter()$iter, balances$getBuffer()$getEndIter()$iter);
			res <- try(parse(text=t), TRUE)
			if(class(res) == "try-error") return(GuiError(w, paste("Invalid formula"), res))
			res <- try(guiEval(ilrName, " <- ", "balance(acomp(", inputName, "),", t,")"), TRUE)
			if(class(res) == "try-error") return(GuiError(w, paste("Cannot do balance with data set", inputName), res))
		}

		guiPrint(ilrName)

		guiSetData("ilrName", ilrName)
		guiCheckStatus()
		w$destroy()

		if(!is.null(callback)) callback()
	})

	# pack the window content to vertical box
	vbox <- gtkVBoxNew()
	vbox$packStart(inputBox, FALSE, FALSE, 0)
	vbox$packStart(entryBoxAlign, FALSE, FALSE, 0)
	vbox$packStart(typeBoxAlign, FALSE, FALSE, 0)
	vbox$packStart(rationingBox, FALSE, FALSE, 0)
	vbox$packStart(balancesAlign, FALSE, FALSE, 0)
	vbox$packStart(loadAlign, FALSE, FALSE, 0)
	w$add(vbox)

	# move that window into the center of main window
	guiWindowToCenter(window, w)
	w$show()
}

