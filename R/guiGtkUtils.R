paste0 <-
function(...) {
	paste(..., sep="")
}

guiActivatedToggleList <-
function(model) {
	indexes = c()
	iter = model$getIterFirst()$iter
	i = 1
	while(T) {
		if(as.logical(model$get(iter, 0))) indexes = c(indexes, i)
		i = i+1
		if(!model$iterNext(iter)) break
	}
	return(indexes)
}

guiComboNew <-
function(vars) {
	# variable combo
	combo <- gtkComboBoxNewText()
	combo$show()
	# every suitable object add to that combo
	for(obj in vars) combo$appendText(obj)
	return(combo)
}

guiInitColumnsPackages <-
function(listStore, title) {
	listView <- gtkTreeViewNewWithModel(listStore)

	rendererText <- gtkCellRendererTextNew()
	listView$insertColumnWithAttributes(-1, title, rendererText, text=0)

	return(listView)
}

guiInitColumnsTreeView <-
function(items, initialStatus, prev, ilrMode, ilrModeSib) {
	ilrList <- gtkTreeViewNewWithModel(ilrMode)
	ilrList$setHeadersVisible(FALSE)

	d = rep(FALSE, length(items))
	if(!is.null(prev)) for(p in prev) d[p] = TRUE
	else d = rep(initialStatus, length(items))

	i = 1
	for(column in items) {
		iter <- ilrMode$append()$iter
		ilrMode$set(iter, 0, d[i], 1, column)
		i = i + 1
	}

	rendererText <- gtkCellRendererTextNew()
	rendererRadio <- gtkCellRendererToggleNew()

	ilrList$insertColumnWithAttributes(-1, "text", rendererRadio, active=0)
	ilrList$insertColumnWithAttributes(-1, "text", rendererText, text=1)

	gSignalConnect(rendererRadio, "toggled", function(renderer, path) {
		pathobj = gtkTreePathNewFromString(path)
		
		iter = ilrMode$getIter(pathobj)$iter
		v = ! as.logical(ilrMode$get(iter, 0))
		ilrMode$set(iter, 0, v)

		if(v == TRUE) {
			iter = ilrModeSib$getIter(pathobj)$iter
			ilrModeSib$set(iter, 0, FALSE)
		}
	})

	return(ilrList)
}

guiInvertToggleList <-
function(model, modelSib) {
	iter = model$getIterFirst()$iter
	iterSib = modelSib$getIterFirst()$iter
	while(T) {
		v = as.logical(model$get(iter, 0))
		model$set(iter, 0, !v)
		if(v == FALSE) {
			modelSib$set(iterSib, 0, FALSE)
		}

		if(!model$iterNext(iter)) break
		modelSib$iterNext(iterSib)
	}
}

guiPaddedLabelNew <-
function(text = "") {
	label <- gtkLabelNew(text)
	label$setPadding(5, 0)
	return(label)
}

guiWindowToCenter <-
function(parent, child) {
	parentsize = parent$getSize()
	parentpos = parent$getPosition()
	childsize = child$getSize()

	# simple analytic geometry :)
	child$move(parentpos$root.x + parentsize$width/2 - childsize$width/2, parentpos$root.y + parentsize$height/2 - childsize$height/2)
}

