#' Predefined simbases in WoodSimulatR
#'
#' @format For statistical simulation of datasets in \code{WoodSimulatR}, one
#' needs a \code{\link{simbase_covar}} object. \code{WoodSimulatR} contains a
#' set of such predefined simbases for Norway spruce (\emph{Picea abies}) grown
#' in Austria.
#'
#' The names of the simbases follow the following schema -- the different parts
#' are separated by "_":
#'
#' \itemize{
#'  \item "ws" -- abbreviation of "\strong{W}oodSimulatR \strong{s}imbase"
#'  \item loadtype -- can either be "t" for material tested in tension,
#'    or "be" for material tested in bending
#'  \item subsample -- empty for the full dataset, "tr" for the part of the
#'    dataset that was used for training, "te" for the part that was used for
#'    testing. The latter two can be used to more closely simulate independent
#'    training and test samples
#'  \item transformation -- empty for no transformation, "logf" if the strength
#'    has been log-transformed prior to calculation of the simbase -- see also
#'    the argument \code{transforms} in \code{\link{simbase_covar}}.
#' }
#'
#' The simbases contain the basis for simulating the following variables:
#'  \describe{
#'    \item{f}{Bending or tension strength, in N/mm².}
#'    \item{E}{Static modulus of elasticity in bending or tension, in N/mm².}
#'    \item{rho}{Density of a small clear sample, in kg/m³.}
#'    \item{E_dyn}{Dynamic modulus of elasticity of the timber after drying to a
#'      moisture content of about 12%, in N/mm².}
#'    \item{E_dyn_u}{Dynamic modulus of elasticity of the timber in the green
#'      state, with moisture contents mostly above fibre saturation point,
#'      in N/mm².}
#'    \item{ip_rho}{An "indicating property" (IP) for density, established by
#'      measuring the weight of each board and dividing by its volume, in kg/m³.}
#'    \item{ip_f}{An "indicating property" (IP) for strength, established by
#'      linear regression on \code{E_dyn}, \code{ip_rho} and a knot parameter
#'      called "total knot area ratio" (tKAR), in N/mm².}
#'  }
#'
#' @source The simbases were created based on data from the research project
#' SiOSiP of Holzforschung Austria. "SiOSiP" is short for "simulation-based
#' optimization of sawn timber production" and ran from 2014 to 2017.
#'
#' @usage
#'  ws_t
#'  ws_t_tr
#'  ws_t_te
#'  ws_t_logf
#'  ws_t_tr_logf
#'  ws_t_te_logf
#'  ws_be
#'  ws_be_tr
#'  ws_be_te
#'  ws_be_logf
#'  ws_be_tr_logf
#'  ws_be_te_logf
#'
#' @name simbase
#' @aliases ws_t ws_t_tr ws_t_te
#'  ws_t_logf ws_t_tr_logf ws_t_te_logf
#'  ws_be ws_be_tr ws_be_te
#'  ws_be_logf ws_be_tr_logf ws_be_te_logf
NULL

#' Means and standard deviations of grade determining properties (GDPs) from literature
#'
#' @details
#' For simulation of an entire dataset with different subsamples with different
#' characteristics (see \code{\link{simulate_dataset}}),
#' it may be useful to be able to refer to existing results
#' from literature as a basis.
#'
#' In the dataset \code{gdp_data}, means and standard deviations for a number of
#' such subsamples have been collected.
#'
#' The GDP values collected in \code{gdp_data} were selected from
#' publications which aimed at representative sampling within the respective
#' countries.
#' All the same, care must be taken when using these values,
#' due to the natural high variability of timber properties.
#'
#' @format
#' \describe{
#'  \item{species}{Wood species as a four letter code according to EN 13556.
#'    Currently, this is always "PCAB" for Norway spruce (\emph{Picea abies}).}
#'  \item{loadtype}{Kind of destructive testing applied to the material --
#'    "t" for material tested in tension, "be" for material tested in bending.}
#'  \item{project}{Research project from which the data originated; \code{"null"}
#'    if unknown or not applicable.}
#'  \item{country}{Country from which the material originated, as a two letter
#'    country code.}
#'  \item{share}{Number of pieces on which the values are based.}
#'  \item{f_mean, f_sd}{Mean and standard deviation of strength, in N/mm².}
#'  \item{E_mean, E_sd}{Mean and standard deviation of the static modulus of
#'    elasticity, in N/mm².}
#'  \item{rho_mean, rho_sd}{Mean and standard deviation of density, in kg/m³.}
#'  \item{literature}{Reference to the literature source; \code{"null"} if not
#'    published yet.}
#'  \item{subsample}{For distinguishing multiple rows with the same species,
#'    loadtype and country -- if there are no duplicates, it is the same as
#'    \code{country}; if there are duplicates, it is \code{country} plus a
#'    suffixed number separated by "_".}
#' }
#'
#' @source The values have been extracted from the following publications:
#'
#' Ranta-Maunus, Alpo, Julia K. Denzler, and Peter Stapel. 2011.
#' \emph{Strength of European Timber. Part 2. Properties of Spruce and Pine
#'  Tested in Gradewood Project.} VTT.
#'
#' Rohanová, Alena, and Erika Nunez. 2014. "Prediction Models of Slovakian
#'  Structural Timber." \emph{Wood Research} 59 (5): 757–69.
#'
#' Stapel, Peter, and Jan-Willem G. van de Kuilen. 2014. “Efficiency of Visual
#'  Strength Grading of Timber with Respect to Origin, Species, Cross Section,
#'  and Grading Rules: A Critical Evaluation of the Common Standards.”
#'  \emph{Holzforschung} 68 (2): 203–16.
#'
"gdp_data"
