guiSetHelp <-
function(msg, windowBind = NULL, initButton = FALSE, callback = NULL) {
	data <- list(
		"clear" = "Removes all texts from the console",
		"datamenu" = "Load and manage your data",
		"loadfile" = "Load compositional datafile from csv or txt format",
		"loadfilefile" = "Select file to load data from",
		"loadr" = "Use already existing datafile from R environment",
		"loadrr" = "Select variable existing in R global environment",
		"loadpackage" = "Load prepared data set from any installed R package",
		"loadpackagepackage" = "Choose an R package and load a data set from this package",
		"loadpackagedata" = "Select data set to use, please use only compositional data sets",
		"loadpackagewindow" = "Load prepared data set from any installed R package",
		"selectcolumns" = "Select compositional parts and non-compositional variables",
		"selectcolumnscolumns" = "Select compositional parts and non-compositional variables",
		"quit" = "Exit the application",
		"exploremenu" = "Use various statistical analysis to explore the data set",
		"methodsmenu" = "Perform statistical methods",
		"plotmenu" = "Generate various plots for your data set",
		"missings" = "Perform imputation of missing values followed by imputation of rounded zeros",
		"logratios" = "Apply log ratio transformation and show results",
		"ilr" = "Isometric log-ratio transformation",
		"clr" = "Centered log-ratio transformation",
		"alr" = "Additive log-ratio transformation",
		"outliers" = "Detect possible outliers",
		"outlierclassifier" = "Classify outliers",
		"helpmenu" = "Further help",
		"helplog" = "All commands executed during this session",
		"init" = "For start, please load data set either from file, package or existing R data set",
		"dataloaded" = "Now use Explore menu to analyse your data set",
		"summary" = "Perform basic summary statistics",
		"pca" = "Graphical component analysis",
		"ternary" = "Plot ternary diagram",
		"quaternary" = "Plot quaternary diagram of four-part compositional data",
		"boxplots" = "Pairwise log-ratio plots with one covariable",
		"boxplotssimple" = "Boxplots of pairwise log-ratios of compositional parts",
		"balances" = "Compute balance in compositional data set",
		"scatterplot" = "Plot scatteplot of log-ratio transformed compositional data with covariate",
		"pairwiseplot" = "Plot scatteplot of log-ratio transformed compositional data with covariate",
		"pwlrplot" = "Show pairwise log-ratio plot of compositional data with covariate"
	)
	topics <- list(
		"summary" = "summary.acomp",
		"missings" = "impCoda",
		"ilr" = "isomLR",
		"clr" = "cenLR",
		"alr" = "addLR",
		"balances" = "balance",
		"logratios" = "ilr",
		"outliers" = "plot.outCoDa",
		"outlierclassifier" = "OutlierClassifier1",
		"ternary" = "plot.acomp",
		"pca" = "pcaCoDa",
		"quaternary" = "plot3D.acomp",
		"boxplots" = "boxplot.acomp",
		"boxplotssimple" = "boxplot.acomp",
		"scatterplot" = "plot.default",
		"pairwiseplot" = "pairwisePlot",
		"pairwiseplot" = "pwlrPlot"
	)

	msgtowrite = if(msg=="main") guiGetData("helpMain") else msg

	guiGetData("helpLabel")$setText(if(is.null(data[[msgtowrite]])) msgtowrite else data[[msgtowrite]])
	if(guiGetData("helpDefault") != msg || !is.null(windowBind) || initButton) {
		guiGetData("f1Label")$setText(if(is.null(callback) && (is.null(windowBind) || is.null(topics[[msgtowrite]]))) "" else "Press F1 for further help")
	}

	if(!is.null(windowBind)) {
		pre = guiGetData("helpDefault")
		guiSetData("helpDefault", msg)

		gSignalConnect(windowBind, "hide", function(arg1, arg2) {
			if(!is.null(pre) && guiGetData("helpDefault") == msg) {
				guiSetHelp(pre)
				guiSetData("helpDefault", pre)
			}
		})
		gSignalConnect(windowBind, "key-press-event", function(widget, event) {
			if(event$keyval == 65470) {
				if(is.function(callback)) msg <- callback()
				if(!is.null(topics[[msg]])) print(help(topics[[msg]], try.all.packages=FALSE, help_type = "html"))
			}
		})

	}

	if(initButton) {
		button <- gtkButtonNewFromStock(GTK_STOCK_HELP)
		gSignalConnect(button, "clicked", function(obj) {
			if(!is.null(callback)) msg <- callback()
			print(help(topics[[msg]], try.all.packages=FALSE, help_type = "html"))
		})
		return(button)
	}
}

guiRegisterHelp <-
function(obj, type) {
	gSignalConnect(obj, "enter-notify-event", function(arg1, arg2) { guiSetHelp(type) })
	gSignalConnect(obj, "leave-notify-event", function(arg1, arg2) { guiSetHelp(guiGetData("helpDefault")) })
}

