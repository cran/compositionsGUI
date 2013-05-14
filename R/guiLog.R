guiS <- 
function(str) {
	return(paste0("\"", gsub("\\\\", "\\\\\\\\", str), "\""))
}

guiEval <- 
function(...) {
	d <- paste0(paste0(list(...), collapse=""), "\n")

	commandLog = guiGetData("commandLog")
	commandLogPre = guiGetData("commandLogPre")
	commandLog$insert(commandLog$getEndIter()$iter, d)
	commandLogPre$insert(commandLogPre$getEndIter()$iter, "> \n")

	guiPrintOnly(paste(">", d, sep=" "))
	return(eval(parse(text=d), envir=.GlobalEnv))
}

guiPrint <-
function(..., replaceNames=FALSE) {
	if(replaceNames) {
		orig = d
		nnn = c()
		i = 1
		for(name in names(d)) {
			nnn = c(nnn, paste0("V", i))
			guiPrintOnly("V", i, ": ", name, "\n")
			i = i + 1
		}
		names(d) <- nnn
		guiPrintOnly("\n");
	}

	output <- capture.output(guiEval(...))
	lines = strsplit(output, "\n");
	lines = c(lines, "")

	printLog = guiGetData("printLog")
	printLogConsole = guiGetData("printLogConsole")

	for(line in lines) {
		printLog$insert(printLog$getEndIter()$iter, paste0(line, "\n"))
	}
	printLog$placeCursor(printLog$getEndIter()$iter);
	printLogConsole$scrollToMark(printLog$getInsert(), 0)

	if(replaceNames) {
		guiPrintOnly("\n");
		i = 1
		for(name in names(orig)) {
			guiPrintOnly("V", i, ": ", name, "\n")
			i = i + 1
		}
	}
}

guiPrintOnly <-
function(...) {
	d <- paste0(paste0(list(...), collapse=""))

	printLog = guiGetData("printLog")
	printLogConsole = guiGetData("printLogConsole")

	printLog$insert(printLog$getEndIter()$iter, d)
	printLog$placeCursor(printLog$getEndIter()$iter)
	printLogConsole$scrollToMark(printLog$getInsert(), 0)
}

