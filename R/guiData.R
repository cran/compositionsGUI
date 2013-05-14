sdcGUIenv <- function() {
	get("sdcGUIenvir", envir=as.environment("package:compositionsGUI"))
}

# put in  GUI env
guiSetData <- function(key, data) {
	assign(key, data, envir=sdcGUIenv())
}

# get from GUI env
guiGetData <- function (key) { 
	if(exists(key, envir=sdcGUIenv(), inherits=FALSE)) get(key, envir=sdcGUIenv(), inherits=FALSE)
	else NULL
}

