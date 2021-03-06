% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.r
\docType{data}
\name{gdp_data}
\alias{gdp_data}
\title{Means and standard deviations of grade determining properties (GDPs) from literature}
\format{
\describe{
\item{species}{Wood species as a four letter code according to EN 13556.
Currently, this is always "PCAB" for Norway spruce (\emph{Picea abies}).}
\item{loadtype}{Kind of destructive testing applied to the material --
"t" for material tested in tension, "be" for material tested in bending.}
\item{project}{Research project from which the data originated; \code{"null"}
if unknown or not applicable.}
\item{country}{Country from which the material originated, as a two letter
country code.}
\item{share}{Number of pieces on which the values are based.}
\item{f_mean, f_sd}{Mean and standard deviation of strength, in N/mm².}
\item{E_mean, E_sd}{Mean and standard deviation of the static modulus of
elasticity, in N/mm².}
\item{rho_mean, rho_sd}{Mean and standard deviation of density, in kg/m³.}
\item{literature}{Reference to the literature source; \code{"null"} if not
published yet.}
\item{subsample}{For distinguishing multiple rows with the same species,
loadtype and country -- if there are no duplicates, it is the same as
\code{country}; if there are duplicates, it is \code{country} plus a
suffixed number separated by "_".}
}
}
\source{
The values have been extracted from the following publications:

Ranta-Maunus, Alpo, Julia K. Denzler, and Peter Stapel. 2011.
\emph{Strength of European Timber. Part 2. Properties of Spruce and Pine
Tested in Gradewood Project.} VTT.

Rohanová, Alena, and Erika Nunez. 2014. "Prediction Models of Slovakian
Structural Timber." \emph{Wood Research} 59 (5): 757–69.

Stapel, Peter, and Jan-Willem G. van de Kuilen. 2014. “Efficiency of Visual
Strength Grading of Timber with Respect to Origin, Species, Cross Section,
and Grading Rules: A Critical Evaluation of the Common Standards.”
\emph{Holzforschung} 68 (2): 203–16.
}
\usage{
gdp_data
}
\description{
Means and standard deviations of grade determining properties (GDPs) from literature
}
\details{
For simulation of an entire dataset with different subsamples with different
characteristics (see \code{\link{simulate_dataset}}),
it may be useful to be able to refer to existing results
from literature as a basis.

In the dataset \code{gdp_data}, means and standard deviations for a number of
such subsamples have been collected.

The GDP values collected in \code{gdp_data} were selected from
publications which aimed at representative sampling within the respective
countries.
All the same, care must be taken when using these values,
due to the natural high variability of timber properties.
}
\keyword{datasets}
