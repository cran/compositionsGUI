GuiLoadFromPackage <-
function(window) {
	# child of the main window
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("Load data set from R package")
	w$setTransientFor(window)
	w$setDestroyWithParent(TRUE)

	packageListStore = gtkListStoreNew("gchararray");
	dataListStore = gtkListStoreNew("gchararray");

	packageView <- guiInitColumnsPackages(packageListStore, "packages")
	dataView <- guiInitColumnsPackages(dataListStore, "data sets")

	items = library()$results[,1]
	i = 0
	for(column in items) {
		iter <- packageListStore$append()$iter
		packageListStore$set(iter, 0, column)
		i = i + 1
	}

	packagescroll = gtkScrolledWindowNew()
	packagescroll$setPolicy("automatic", "automatic")
	packagescroll$add(packageView)
	datascroll = gtkScrolledWindowNew()
	datascroll$setPolicy("automatic", "automatic")
	datascroll$add(dataView)

	guiRegisterHelp(packageView, "loadpackagepackage")
	guiRegisterHelp(dataView, "loadpackagedata")
	guiSetHelp("loadpackagewindow", w)

	hboxtop = gtkHBoxNew()
	hboxtop$packStart(packagescroll, TRUE, TRUE)
	hboxtop$packStart(datascroll, TRUE, TRUE)

	okbutton <- gtkButtonNewWithLabel("OK")
	okbutton$setSizeRequest(60,30)
	okbuttonbox <- gtkHBoxNew()
	okbuttonbox$packEnd(okbutton, FALSE, FALSE, 3)

	vbox = gtkVBoxNew()
	vbox$packStart(hboxtop, TRUE, TRUE, 3)
	vbox$packStart(okbuttonbox, FALSE, TRUE, 3)

	w$resize(400, 400)
	w$add(vbox)
	w$show()

	gSignalConnect(packageView, "cursor-changed", function(obj) {
		dataListStore$clear()

		path = packageView$getCursor()$path
		if(!is.null(path)) {
			iter = packageListStore$getIter(path)$iter
			curText = as.character(packageListStore$get(iter, 0))

			items = data(package=curText)$results[,3]	

			i = 0
			for(column in items) {
				iter <- dataListStore$append()$iter
				dataListStore$set(iter, 0, column)
				i = i + 1
			}
		}
	})

	gSignalConnect(okbutton, "clicked", function(obj) {
		pathPackage = packageView$getCursor()$path
		pathData = dataView$getCursor()$path

		if(!is.null(pathPackage) && !is.null(pathData)) {
			iter = packageListStore$getIter(pathPackage)$iter
			packageName = as.character(packageListStore$get(iter, 0))
			iter = dataListStore$getIter(pathData)$iter
			dataName = as.character(dataListStore$get(iter, 0))

			dataName = unlist(strsplit(dataName, "\\s"))[1]
			realDataName = unlist(strsplit(dataName, ".", TRUE))[1]

			res = try(guiEval("data(list=", guiS(realDataName), ", package=", guiS(packageName),")"), TRUE)
			if(class(res) == "try-error") return(GuiError(w, "Cannot load data set", res))

			res = get(dataName, envir=.GlobalEnv)
			if(class(res) == "try-error") return(GuiError(w, "Cannot get data set", res))

			if(!guiCheckData(res)) return(GuiError(w, paste("Datafile has invalid format (or has only one row or column)")))

			guiSetData("compIndexes", NULL)
			guiSetData("externalIndexes", NULL)
			guiCheckStatus()
			
			w$destroy()
			return(GuiInitVariables(window, dataName))
		}
	})
}
