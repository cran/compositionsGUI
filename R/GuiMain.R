GuiMain <-
function() {
	# main window initialization, not shown at this moment
	window <- gtkWindowNew(NULL, FALSE);
	# title of that window
	window$setTitle("compositionsGUI");

	# list of actions with accelerator keys and labels
	actions <- list(
			list("DataMenuAction", NULL, "_Data"),
			list("ExploreMenuAction", NULL, "_Explore"),
			list("MethodsMenuAction", NULL, "_Methods"),
			list("PlotMenuAction", NULL, "_Plot"),
			list("HelpAction", NULL, "_Help"),
			list("DescriptiveDiagramsAction", NULL, "_Descriptive diagrams"),
			list("CovariateDiagramsAction", NULL, "_Diagrams with covariates"),
			list("LoadFileAction", "gtk-open", "_Load file ...", "<control>O", "Read data set from external file", function(action, window) {
				GuiLoadFile(window)
			}),
			list("LoadVariableAction", NULL, "Load data from R", "<control>V", "Read data set from existing R data set", function(action, window) {
				GuiLoadVariable(window)
			}),
			list("SelectColumnsAction", NULL, "Select variables", "<control>C", "Select columns for compositional parts and external variables of the data set", function(action, window) {
				GuiInitVariables(window)
			}),
			list("SummaryAction", NULL, "Summary statistics", "<control>U", "Summary statistics", function(action, window) {
				GuiSummary(window)
			}),
			list("ComputeIlrAction", NULL, "Log-ratio transformations", "<control>I", "Log-ratio transformations", function(action, window) {
				GuiIlr(window)
			}),
			list("ComputeImpAction", NULL, "Imputation of missing values and values below DL", "<control>J", "Imputation of missing values and values below DL", function(action, window) {
				GuiImp(window)
			}),
			list("ComputeOuterAction", NULL, "Outlier detection", "<control>D", "Outlier detection", function(action, window) {
				GuiOutCoDa(window)
			}),
			list("ComputeClassifierAction", NULL, "Outlier classification", "<control>F", "Outlier classification", function(action, window) {
				GuiOutClass(window)
			}),
			list("PCAAction", NULL, "Principal component analysis", "<control>A", "Principal component analysis", function(action, window) {
				GuiPca(window)
			}),
			list("TernaryAction", NULL, "Ternary diagram", "<control>T", "Ternary diagram", function(action, window) {
				GuiTernary(window)
			}),
			list("QuaternaryAction", NULL, "Quaternary diagram", "<control>R", "Quaternary diagram", function(action, window) {
				GuiQuaternary(window)
			}),
			list("ScatterplotAction", NULL, "Scatterplot of transformed data", "<control>M", "Scatterplot of transformed data", function(action, window) {
				GuiScatter(window)
			}),
			list("BoxplotsAction", NULL, "Boxplots of pairwise log-ratios", "<control>S", "Boxplots of pairwise log-ratios", function(action, window) {
				GuiBoxplot(window)
			}),
			list("CovariateScatterplotAction", NULL, "Scatterplots with covariates", "<control>E", "Scatterplots with covariates", function(action, window) {
				GuiPairwise(window)
			}),
			list("PwlrAction", NULL, "Pairwise log-ratio plot with covariates", "<control>Y", "Pairwise log-ratio plot with covariates", function(action, window) {
				GuiPwlr(window)
			}),
			list("QuitAction", "gtk-quit", "_Quit", "<control>Q", "Quit compisitionsGUI", function(action, window) {
				guiQuit(window)
			}),
			list("LogAction", NULL, "Show command log", "<control>L", "Show command log", function(action, window) {
				GuiCommandLog(window)
			}),
			list("LoadFromPackageAction", NULL, "Load data set from an installed R package", "<control>P", "Load from package", function(action, window) {
				GuiLoadFromPackage(window)
			})
	)

	# layout of the menubar object
	menus <- "
	<ui>
	<menubar name='MenuBar'>
	<menu name='DataMenu' action='DataMenuAction'>
	<menuitem name='LoadFile' action='LoadFileAction' />
	<menuitem name='LoadVariable' action='LoadVariableAction' />
	<menuitem name='LoadFromPackage' action='LoadFromPackageAction' />
	<menuitem name='SelectColumns' action='SelectColumnsAction' />
	<menuitem name='Quit' action='QuitAction' />
	</menu>
	<menu name='ExploreMenu' action='ExploreMenuAction'>
	<menuitem name='Summary' action='SummaryAction' />
	<menuitem name='ComputeImp' action='ComputeImpAction' />
	<menuitem name='ComputeIlr' action='ComputeIlrAction' />
	</menu>
	<menu name='MethodsMenu' action='MethodsMenuAction'>
	<menuitem name='ComputeOuter' action='ComputeOuterAction' />
	<menuitem name='ComputeClassifier' action='ComputeClassifierAction' />
	<menuitem name='PCA' action='PCAAction' />
	</menu>
	<menu name='PlotMenu' action='PlotMenuAction'>
	<menu name='DescriptiveDiagramsMenu' action='DescriptiveDiagramsAction'>
	<menuitem name='Ternary' action='TernaryAction' />
	<menuitem name='Quaternary' action='QuaternaryAction' />
	<menuitem name='Scatterplot' action='ScatterplotAction' />
	<menuitem name='Boxplots' action='BoxplotsAction' />
	</menu>
	<menu name='CovariateDiagramsMenu' action='CovariateDiagramsAction'>
	<menuitem name='Pwlr' action='PwlrAction' />
	<menuitem name='CovariateScatterplot' action='CovariateScatterplotAction' />
	</menu>
	</menu>
	<menu name='HelpMenu' action='HelpAction'>
	<menuitem name='Log' action='LogAction' />
	</menu>
	</menubar>
	</ui>"

	# action group for main window
	actionGroup <- gtkActionGroupNew("MainWindowActions")
	actionGroup$addActionsFull(actions, window)

	# action manager handles all the actions we define for the window
	manager <- gtkUIManagerNew()
	manager$insertActionGroup(actionGroup, 0)
	manager$addUiFromString(menus)
	window$addAccelGroup(manager$getAccelGroup())

	# and can provide us with menubar Widget
	menubar <- manager$getWidget("/MenuBar")

	# status summary
	statusLabel <- guiPaddedLabelNew()
	statusLabelAlign <- gtkAlignmentNew(0,0,0,0)
	statusLabelAlign$add(statusLabel)

	# the output console
	printLog <- gtkTextBufferNew()
	console <- gtkTextViewNewWithBuffer(printLog)
	console$modifyFont(pangoFontDescriptionFromString("monospace normal 9"))
	console$setEditable(FALSE)

	consolescroll = gtkScrolledWindowNew()
	consolescroll$setPolicy("automatic", "automatic")
	consolescroll$add(console)

	# clear button
	f1text = gtkLabelNew("")
	f1align = gtkAlignmentNew(0, 0, 0, 0)
	f1align$setPadding(5,5,5,5)
	f1align$add(f1text)

	clear <- gtkButtonNewWithLabel("clear")
	clearBox <- gtkHBoxNew()
	clearBox$packStart(f1align, TRUE, TRUE, 0)
	clearBox$packStart(clear, FALSE, FALSE, 0)
	gSignalConnect(clear, "clicked", function(obj) {
		guiGetData("printLog")$setText("")
	})

	helptext = gtkLabelNew("")
	helptext$setLineWrap(TRUE)
	gtkMiscSetAlignment(helptext, 0, 0)
	helpbox = gtkHBoxNew()
	helpbox$setSizeRequest(-1, 40)
	helpbox$packStart(helptext, FALSE, FALSE, 5)

	guiRegisterHelp(clear, "clear")
	guiRegisterHelp(manager$getWidget("/MenuBar/DataMenu"), "datamenu")
	guiRegisterHelp(manager$getWidget("/MenuBar/DataMenu/LoadFile"), "loadfile")
	guiRegisterHelp(manager$getWidget("/MenuBar/DataMenu/LoadVariable"), "loadr")
	guiRegisterHelp(manager$getWidget("/MenuBar/DataMenu/LoadFromPackage"), "loadpackage")
	guiRegisterHelp(manager$getWidget("/MenuBar/DataMenu/SelectColumns"), "selectcolumns")
	guiRegisterHelp(manager$getWidget("/MenuBar/DataMenu/Quit"), "quit")
	guiRegisterHelp(manager$getWidget("/MenuBar/ExploreMenu"), "exploremenu")
	guiRegisterHelp(manager$getWidget("/MenuBar/ExploreMenu/Summary"), "summary")
	guiRegisterHelp(manager$getWidget("/MenuBar/ExploreMenu/ComputeImp"), "missings")
	guiRegisterHelp(manager$getWidget("/MenuBar/ExploreMenu/ComputeIlr"), "logratios")
	guiRegisterHelp(manager$getWidget("/MenuBar/MethodsMenu"), "methodsmenu")
	guiRegisterHelp(manager$getWidget("/MenuBar/MethodsMenu/ComputeOuter"), "outliers")
	guiRegisterHelp(manager$getWidget("/MenuBar/MethodsMenu/ComputeClassifier"), "outlierclassifier")
	guiRegisterHelp(manager$getWidget("/MenuBar/MethodsMenu/PCA"), "pca")
	guiRegisterHelp(manager$getWidget("/MenuBar/PlotMenu"), "plotmenu")
	guiRegisterHelp(manager$getWidget("/MenuBar/PlotMenu/DescriptiveDiagramsMenu/Ternary"), "ternary")
	guiRegisterHelp(manager$getWidget("/MenuBar/PlotMenu/DescriptiveDiagramsMenu/Quaternary"), "quaternary")
	guiRegisterHelp(manager$getWidget("/MenuBar/PlotMenu/DescriptiveDiagramsMenu/Scatterplot"), "scatterplot")
	guiRegisterHelp(manager$getWidget("/MenuBar/PlotMenu/DescriptiveDiagramsMenu/Boxplots"), "boxplots")
	guiRegisterHelp(manager$getWidget("/MenuBar/PlotMenu/CovariateDiagramsMenu/Pwlr"), "pwlrplot")
	guiRegisterHelp(manager$getWidget("/MenuBar/PlotMenu/CovariateDiagramsMenu/CovariateScatterplot"), "pairwiseplot")
	guiRegisterHelp(manager$getWidget("/MenuBar/HelpMenu"), "helpmenu")
	guiRegisterHelp(manager$getWidget("/MenuBar/HelpMenu/Log"), "helplog")

	# pack window content with menu into vertical box
	windowBox <- gtkVBoxNew()
	windowBox$packStart(menubar, FALSE, FALSE, 0)
	windowBox$packStart(statusLabelAlign, FALSE, FALSE, 0)
	windowBox$packStart(consolescroll, TRUE, TRUE, 0)
	windowBox$packStart(clearBox, FALSE, FALSE, 0)
	windowBox$packStart(helpbox, FALSE, FALSE, 0)
	window$add(windowBox)

	# window is going to be visible now
	window$resize(600, 400)
	window$show()

	# main window widget
	guiSetData("mainWindow", window)
	# action manager for main window
	guiSetData("mainActionManager", manager)
	# status label
	guiSetData("dataStatusLabel", statusLabel)
	# help text Label
	guiSetData("helpLabel", helptext)
	guiSetData("f1Label", f1text)
	# textbuffer for the command list
	guiSetData("commandLog", gtkTextBufferNew())
	guiSetData("commandLogPre", gtkTextBufferNew())
	# text buffer for main window console
	guiSetData("printLog", printLog)
	guiSetData("printLogConsole", console)

	guiSetData("helpMain", "init")
	guiSetHelp("main", window)
	guiCheckStatus()
}

guiCheckStatus <-
function() {
	manager = guiGetData("mainActionManager")
	if(!is.null(manager)) {
		# select columns will be available only if data are loaded
		manager$getWidget("/MenuBar/DataMenu/SelectColumns")$setSensitive(!is.null(guiGetData("originalName")))
		manager$getWidget("/MenuBar/ExploreMenu/Summary")$setSensitive(!is.null(guiGetData("compName")))
		manager$getWidget("/MenuBar/ExploreMenu/ComputeIlr")$setSensitive(!is.null(guiGetData("compName")))
		manager$getWidget("/MenuBar/ExploreMenu/ComputeImp")$setSensitive(!is.null(guiGetData("compName")))
		manager$getWidget("/MenuBar/MethodsMenu/ComputeOuter")$setSensitive(!is.null(guiGetData("compName")))
		manager$getWidget("/MenuBar/MethodsMenu/ComputeClassifier")$setSensitive(!is.null(guiGetData("compName")))
		manager$getWidget("/MenuBar/MethodsMenu/PCA")$setSensitive(!is.null(guiGetData("compName")))
		manager$getWidget("/MenuBar/PlotMenu/DescriptiveDiagramsMenu/Ternary")$setSensitive(!is.null(guiGetData("compName")))
		manager$getWidget("/MenuBar/PlotMenu/DescriptiveDiagramsMenu/Quaternary")$setSensitive(!is.null(guiGetData("compName")))
		manager$getWidget("/MenuBar/PlotMenu/DescriptiveDiagramsMenu/Scatterplot")$setSensitive(!is.null(guiGetData("compName")))
		manager$getWidget("/MenuBar/PlotMenu/DescriptiveDiagramsMenu/Boxplots")$setSensitive(!is.null(guiGetData("compName")))
		manager$getWidget("/MenuBar/PlotMenu/CovariateDiagramsMenu/Pwlr")$setSensitive(!is.null(guiGetData("compName")))
		manager$getWidget("/MenuBar/PlotMenu/CovariateDiagramsMenu/CovariateScatterplot")$setSensitive(!is.null(guiGetData("compName")))
	}

	guiDataSummary()
}

guiQuit <-
function(window) {
	window$destroy()
}

