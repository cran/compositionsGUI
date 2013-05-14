compositionsGUI <-
function() {
	GuiMain()
}

.onAttach <-
function(libname, pkgname) {
	assign("sdcGUIenvir", new.env(), envir=as.environment("package:compositionsGUI"))
	compositionsGUI()
}
