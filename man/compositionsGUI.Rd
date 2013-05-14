\name{compositionsGUI}
\alias{compositionsGUI}
\title{
Loading the Graphical User Interface for Compositional Data Analysis
}
\description{
A function to start the graphical user interface for compositional data 
analysis.
}
\usage{
compositionsGUI()
}
\value{
NULL
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
\author{
Jiri Eichler, Karel Hron, Raimon Tolosana-Delgado, Gerald van den Boogaart, 
Matthias Templ, Peter Filzmoser
}
\examples{
##loading the graphical user interface package
##and the GUI at the same time
\dontrun{
library(compositionsGUI)

##re-loading of the graphical user interface
compositionsGUI()
}
}
\keyword{environment}
