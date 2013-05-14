GuiCommandLog <-
function(window) {
	w <- gtkWindowNew(NULL, FALSE)
	w$setTitle("Command log")
	# is child of the main window
	w$setTransientFor(window)
	w$setDestroyWithParent(TRUE)

	guiSetHelp("helplog", w)

	viewpre <- gtkTextViewNewWithBuffer(guiGetData("commandLogPre"))
	viewpre$modifyFont(pangoFontDescriptionFromString("monospace normal 9"))
	viewpre$setEditable(FALSE)

	view <- gtkTextViewNewWithBuffer(guiGetData("commandLog"))
	view$modifyFont(pangoFontDescriptionFromString("monospace normal 9"))
	view$setEditable(FALSE)

        hbox <- gtkHBoxNew()
        hbox$packStart(viewpre, FALSE, FALSE, 0)
        hbox$packStart(view, TRUE, TRUE, 0)

	consolescroll = gtkScrolledWindowNew()
	consolescroll$setPolicy("automatic", "automatic")
	consolescroll$addWithViewport(hbox)
	
	w$add(consolescroll)
	w$resize(600, 400)
	w$show()
}

