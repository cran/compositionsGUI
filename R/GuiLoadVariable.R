# shows the dialog to select internal R variable
GuiLoadVariable <- function(window) {
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("Load data from R data set")
	# is child of the main window
	w$setTransientFor(window)
	w$setDestroyWithParent(TRUE)
	# no reason to do this
	w$setResizable(FALSE)

	guiSetHelp("loadrr", w)

	combo <- guiVariableComboNew(guiVariableList(TRUE))
	combo$setSizeRequest(250, -1)

	# first create the HBox, then pack it into the window's VBox (windowBox)
	comboBox <- gtkHBoxNew()
	comboBox$packStart(guiPaddedLabelNew("data set:"), FALSE, FALSE, 0)
	comboBox$packStart(combo, TRUE, TRUE, 0)

	# the 'load' button
	load <- gtkButtonNewWithLabel("Load")

	gSignalConnect(load, "clicked", function(d) {
		# get the variable from combo
		variable <- combo$getActiveText()
		# only if filled we will do anything
		if(nchar(variable) > 0) {
			# try to get it
			res = try(guiGetVariable(variable), TRUE)
			# failure, show error dialog and DONT close the window
			if(class(res) == "try-error") {
				return(GuiError(w, paste("Cannot load data set", variable), res))
			}
			res2 = try(guiCheckData(res))
			if(class(res2) == "try-error") {
				return(GuiError(w, "Variable has invalid format", res2))
			}

			# data is stored
			guiSetData("compIndexes", NULL)
			guiSetData("externalIndexes", NULL)
			guiSetData("originalIndexes", NULL)
			guiSetData("compName", NULL)
			guiSetData("impName", NULL)
			guiSetData("ilrName", NULL)
			guiCheckStatus()

			# destroy that window
			w$destroy()
			# init window to select columns
			GuiInitVariables(window, variable)
		}
	})

	# pack the window content to vertical box
	vbox <- gtkVBoxNew()
	vbox$packStart(comboBox, FALSE, FALSE, 0)
	vbox$packStart(load, FALSE, FALSE, 0)
	w$add(vbox)

	# move that window into the center of main window
	guiWindowToCenter(window, w)
	w$show()
}

# returns the variable by its name
guiGetVariable <- function(name) {
	if(!exists(name)) {
		guiEval("data(list=c(", name, "))")
	}
	return(get(name, envir=.GlobalEnv))
}

