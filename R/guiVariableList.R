guiVariableList <-
function(loadPackage = FALSE) {
	objs = c()

	for (name in objects(".GlobalEnv")) {
		obj <- get(name, envir=.GlobalEnv)
		res = try(guiCheckData(obj), TRUE)
		if(class(res) != "try-error") objs = c(objs, name)
	}
	# and add all objects from our package
	if(loadPackage) for(name in data(package="robCompositions")[3]$results[, 3]) {
		objs = c(objs, name)
	}

	return(unique(objs))
}

guiExternalsList <-
function(continuous = FALSE) {
	objs = c()

	prefer = guiGetData("externalName")
	obj = try(get(prefer, .GlobalEnv), TRUE)
	if(class(obj) != "try-error") {
		if(!is.null(ncol(obj))) {
			for(colname in colnames(obj)) {
				if(xor(!is.numeric(obj[[colname]]) && !is.integer(obj[[colname]]), continuous)) {
					objs = c(objs, paste0(prefer, "$", colname))
				}
				
			}
		} else if(is.vector(obj)) {
			objs = c(objs, prefer)
		}
	}

	for (name in objects(".GlobalEnv")) {
		if(name != prefer) {
			obj <- get(name, envir=.GlobalEnv)
			if(!is.null(ncol(obj))) {
				for(colname in colnames(obj)) {
					if(xor(!is.numeric(obj[[colname]]) && !is.integer(obj[[colname]]), continuous)) {
						objs = c(objs, paste0(name, "$", colname))
					}
				}
			} else if(is.vector(obj)) {
				objs = c(objs, name)
			}
		}
	}

	return(unique(objs))
}

guiGetDefaultExternal <- function(continuous = FALSE) {
	ll <- guiExternalsList(continuous)
	if(length(ll) > 0) return(ll[1])
	else return("")
}

