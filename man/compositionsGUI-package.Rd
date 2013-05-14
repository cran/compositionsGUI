\name{compositionsGUI-package}
\alias{compositionsGUI-package}
\alias{Patterns}
\alias{print.Patterns}
\alias{impAll}
\alias{pairwisePlot}
\alias{pwlrPlot}
\docType{package}
\title{
Graphical user environment for compositional data analysis
}
\description{
The package provides a graphical user interface and a teaching tool 
for compositional data analysis using R. It loads data from various 
sources, and contains functions for preprocessing, basic summary statistics 
as well as log-ratio
transformations, necessary for a reasonable statistical treatment of compositional 
data. Further, also statistical methods like outlier detection and principal 
component analysis, together with the corresponding graphical tools, are provided. 
Finally, the package contains plotting functions, suitable for these data,
like ternary and quaternary diagrams, scatteplots of log-ratio 
transformed data, and diagrams with categorical/continuous covariates.
Both classical and robust versions of (almost) all methods are available.
This package builds upon the R packages "robCompositions" and "compositions".
The GUI generates and runs code snippets built from functions of these packages.
Active users can then copy, recycle and improve the code snippets, and
at the same time learn to use the relevant "robCompositions" or "compositions" 
functions.
}
\details{
\tabular{ll}{
Package: \tab compositionsGUI\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-01-09\cr
License: \tab GPL (>=2)\cr
}
}
\author{
Jiri Eichler, Karel Hron, Raimon Tolosana-Delgado, Gerald van den Boogaart, 
Matthias Templ, Peter Filzmoser 

Maintainer: Jiri Eichler <eichler@subreg.cz>
}
\seealso{
  \link{compositions-package},\link{robCompositions-package}
}
\references{
J. Aitchison (1986) The statistical analysis of compositional data. 
Chapman and Hall, London.
V. Pawlowsky-Glahn, A. Buccianti (2011) Compositional data analysis: 
Theory and applications. Wiley, Chichester.
M. Templ, K. Hron, P. Filzmoser (2011) robCompositions: An R-package for robust
statistical analysis of compositional data. In: V. Pawlowsky-Glahn, A. Buccianti, 
Compositional data analysis: Theory and applications. Wiley, Chichester, 341-355.
K.G. van den Boogaart, R. Tolosana-Delgado (2013) Analyzing compositional data
with R. Springer, Heidelberg.
}
\keyword{compositionsGUI}
\examples{
##loading the graphical user interface package
##and the GUI at the same time
\dontrun{
library(compositionsGUI)

##re-loading of the graphical user interface
compositionsGUI()
}
}

