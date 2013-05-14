guiVariableComboNew <-
function(vars, active = NULL) {
	# variable combo
	combo <- gtkComboBoxEntryNewText()
	combo$show()
	# every suitable object add to that combo
	i = 0
	for(obj in vars) {
		combo$appendText(obj)
		if(!is.null(active) && active == obj) combo$setActive(i)
		i = i+1
	}
	return(combo)
}

