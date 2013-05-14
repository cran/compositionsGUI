guiParseFile <-
function(window, filename, funs) {
	# we will try every function
	for(fun in funs) {
		d = try(get(fun, envir=.GlobalEnv)(filename, dec="."), TRUE)
		if(class(d) != "try-error" && class(try(guiCheckData(d), TRUE)) != "try-error") {
			originalName = ".file"

			d2 = get(fun, envir=.GlobalEnv)(filename, dec=",")
			if(sum(lapply(d, class) == "factor") > sum(lapply(d2, class) == "factor")) {
				guiEval(originalName, " <- ", fun, "(", guiS(filename), ", dec=\",\")")
			} else {
				guiEval(originalName, " <- ", fun, "(", guiS(filename), ", dec=\".\")")
			}

			guiSetData("compIndexes", NULL)
			guiSetData("externalIndexes", NULL)
			guiCheckStatus()

			return(GuiInitVariables(window, originalName))
		}
	}

	return(GuiError(window, "Invalid data format"))
}

