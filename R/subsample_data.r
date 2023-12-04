#' Retrieve descriptive data for samples from literature
#'
#' In the \code{WoodSimulatR} package, means and standard deviations of grade
#' determining properties (GDPs) for a number of Norway spruce
#' (\emph{Picea abies}) samples from literature are stored for use in
#' \code{\link{simulate_dataset}}. They are indexed by a two-letter country code
#' (and a suffixed number if disambiguation is required).
#'
#' The direct descriptive data can also be directly accessed
#' (\code{\link{gdp_data}}).
#' The present function is meant to prepare the data
#' as input to the \code{subsets} argument of \code{\link{simulate_dataset}}.
#' It allows picking multiple samples from the same country (disambiguating by
#' creating appropriately named entries in the column \code{subsample}) and
#' creating random sample data (uniformly distributed within the
#' range of values given in the full dataset \code{\link{gdp_data}}
#' for the respective \code{loadtype} and \code{species}) for sample names not
#' found in this dataset.
#'
#' The dataset \code{\link{gdp_data}} contains a column \code{share} which gives
#' the number of pieces in the original sample. Unless relative subsample sizes
#' are explicitly asked for by providing a named numeric vector for the
#' argument \code{country}, the present function always resets \code{share} to
#' 1, prompting \code{\link{simulate_dataset}} to create
#' (approximately) equal-sized subsamples.
#'
#' The GDPs depend on the type of destructive testing done
#' (\code{loadtype}) -- therefore, giving the proper \code{loadtype} is
#' required for realistic values.
#'
#' If \code{country} is \code{NULL} (or omitted), the full dataset
#' \code{\link{gdp_data}} for the
#' respective \code{loadtype} (and \code{species}) is returned.
#'
#' For sample names not contained in the internal list, a warning is issued
#' and random sample data is returned (uniformly distributed within the
#' range of values given in the full table for the respective \code{loadtype}
#' and \code{species}).
#'
#' If \code{country} is just a number (and *not* a named vector), also random
#' sample data is returned; the different "countries" are then named "C1", "C2"
#' and so on.
#'
#' @param country Can be either the number of desired samples, or a
#'  named vector of relative subsample sizes where the names can be
#'  abbreviations of country names. Alternatively, \code{country} can also
#'  be a character vector of country abbreviations.
#' @param loadtype Can be either \code{"be"} for "bending edgewise" or
#'  \code{"t"} for "tension".
#' @param species A species code according to EN 13556:2003. Currently, only
#'  'PCAB' (\emph{Picea abies} = Norway spruce) is supported.
#'
#' @return A data frame with country and subsample names,
#'  relative subsample sizes and some meta-information like project and
#'  literature references, as well as mean
#'  strength and standard deviation of strength, static modulus of elasticity and
#'  density.
#'
#' @section Notes:
#'
#' The GDP values collected in \code{\link{gdp_data}} were selected from
#' publications which aimed at representative sampling within the respective
#' countries.
#' All the same, care must be taken when using these values,
#' due to the natural high variability of timber properties.
#'
#' @examples
#'
#' # get all subsample data for loadtype bending, or tension
#' get_subsample_definitions()
#' get_subsample_definitions(loadtype='be')
#'
#' # get six random samples, explicitly state loadtype tension
#' get_subsample_definitions(country=6, loadtype='t')
#'
#' # get subsample data for the German tension sample in different ways
#' get_subsample_definitions(country='de', loadtype='t')
#' get_subsample_definitions(country=c(de=1), loadtype='t')
#' get_subsample_definitions(country=c(de=6), loadtype='t')
#'
#' # bending samples from Sweden (both samples), Poland, and France, equally
#' # weighted
#' get_subsample_definitions(c('se', 'se_1', 'pl', 'fr'))
#' get_subsample_definitions(c(se=1, se_1=1, pl=1, fr=1))
#' get_subsample_definitions(c(se=5, se_1=5, pl=5, fr=5))
#'
#' # four tension samples from Romania, two from Ukraine and one from Slovakia,
#' # weighted so that each country contributes equally
#' get_subsample_definitions(c(ro=1, ro=1, ro=1, ro=1, ua=2, ua=2, sk=4), loadtype='t')
#'
#' # non-existant subsample names get replaced by random values (which are based
#' # on the range of stored values for the respective loadtype)
#' get_subsample_definitions(c('xx', 'yy', 'zz'))
#' get_subsample_definitions(c('xx', 'yy', 'zz'), loadtype='t')
#'
#' # subsample names are case-sensitive!
#' get_subsample_definitions(c('at', 'aT', 'At', 'AT'), loadtype='t')
#'
#' @importFrom dplyr  group_by
#' @importFrom dplyr  left_join
#' @importFrom dplyr  mutate
#' @importFrom dplyr  n
#' @importFrom dplyr  select
#' @importFrom dplyr  summarise
#' @importFrom dplyr  ungroup
#' @importFrom rlang  .data
#' @importFrom tibble tibble
#' @importFrom stats  runif
#'
#' @export
get_subsample_definitions <- function(country=NULL, loadtype='t', species='PCAB') {
  share <- subsample <- NULL; # due to NSE notes in R CMD check

  # extract data for the loadtype
  stopifnot(loadtype %in% c('be', 't'));
  stopifnot(species %in% c('PCAB'));
  gdps <- WoodSimulatR::gdp_data;
  gdps <- gdps[gdps$loadtype == loadtype & gdps$species == species, ];
  stopifnot(all(c('country', 'subsample', 'share', 'f_mean', 'f_sd') %in% names(gdps)))

  # country empty --> return full list
  if (is.null(country)) {
    gdps$share <- 1;
  } else {

    # transform character vector to named vector of ones
    if (is.character(country)) {
      n <- country;
      country <- rep(1, length(n))
      names(country) <- n;
    }

    # make sure we now have a numeric country vector
    stopifnot(is.numeric(country)); # TODO: improve error message
    stopifnot(all(country > 0)); # TODO: improve error message

    if (length(country) == 1 && is.null(names(country))) {
      # parameter is number of countries -- all get same share
      country <- rep(1, country);
    }

    # make sure all country entries are named
    if (is.null(names(country))) {
      names(country) <- paste0('C', 1 : length(country));
    }
    n <- names(country);
    if (any(n == '')) {
      names(country) <- ifelse(
        n == '',
        paste0('C', 1 : length(country)),
        names(country)
      );
    }

    # storing ranges of f_mean and f_sd for simulation
    f_mean    <- range(gdps$f_mean);
    f_cov     <- range(gdps$f_sd / gdps$f_mean);
    E_mean    <- range(gdps$E_mean);
    E_cov     <- range(gdps$E_sd / gdps$E_mean);
    rho_mean  <- range(gdps$rho_mean);
    rho_cov   <- range(gdps$rho_sd / gdps$rho_mean);

    # getting data for existing country names
    # XXX note on devtools::check: (http://r-pkgs.had.co.nz/check.html):
    #  "codetools::checkUsagePackage() is called to check that your functions
    #   don’t use variables that don’t exist. This sometimes raises false
    #   positives with functions that use non-standard evaluation (NSE), like
    #   subset() or with(). Generally, I think you should avoid NSE in package
    #   functions, and hence avoid this NOTE, but if you can not, see
    #   ?globalVariables for how to suppress this NOTE."
    # NOTE: https://dplyr.tidyverse.org/articles/programming.html says
    #  "[...] using .data also prevents R CMD check from giving a NOTE about
    #   undefined global variables (provided that you’ve also imported
    #   rlang::.data with @importFrom rlang .data).

    # XXX what if we have multiple entries for one country in our gdps table?
    #   then none of the subsample entries would match...
    # --> this is avoided by having one entry in gdps where the subsample
    # equals the country, and only start with "_i" at the second subsample from
    # one country (see Sweden for loadtype bending)
    gdps <- dplyr::left_join(
      tibble::tibble(subsample = names(country), share=country),
      dplyr::select(gdps, -share),
      by='subsample')

    # simulating data for missing subsample names
    i <- is.na(gdps$country);
    if (sum(i) > 0) {
      n_subsets <- sum(i);
      gdps$loadtype[i] <- loadtype;
      gdps$country[i]   <- gdps$subsample[i]; #paste0('C', 1 : n_subsets);
      gdps$f_mean[i]    <- stats::runif(n_subsets, f_mean[1],   f_mean[2]);
      gdps$f_sd[i]      <- stats::runif(n_subsets, f_cov[1],    f_cov[2]) * gdps$f_mean[i];
      gdps$E_mean[i]    <- stats::runif(n_subsets, E_mean[1],   E_mean[2]);
      gdps$E_sd[i]      <- stats::runif(n_subsets, E_cov[1],    E_cov[2]) * gdps$E_mean[i];
      gdps$rho_mean[i]  <- stats::runif(n_subsets, rho_mean[1], rho_mean[2]);
      gdps$rho_sd[i]    <- stats::runif(n_subsets, rho_cov[1],  rho_cov[2]) * gdps$rho_mean[i];
      gdps$literature[i] <- '<simulated>';
      gdps$species[i] <- species;
    }

    # enforce unique subsample names, based on country names if the subsample
    # exists in the stored dataset WoodSimulatR:::gdp_data
    # If a certain subsample name would appear multiple times, indices are
    # appended with '_'. This differs from make.unique in that the first, not
    # the second appearance of the name gets changed to '*_1'.
    gdps$subsample[!i] <- gdps$country[!i];

    dups <- dplyr::summarise(dplyr::group_by(gdps, subsample), n=dplyr::n());
    nums <- dplyr::mutate(dplyr::group_by(gdps, subsample), i=1, i=cumsum(i));
    nums <- dplyr::left_join(dplyr::select(nums, subsample, i), dups, by='subsample');
    nums <- dplyr::mutate(nums, s2 = ifelse(n==1, subsample, paste0(subsample, '_', i)));
    gdps$subsample <- nums$s2;
    gdps$project <- NULL;
  }

  dplyr::ungroup(gdps);
}
