#' Generate an artificial dataset with correlated variables
#'
#' Generate an artificial dataset with correlated variables and defined means
#' and standard deviations.
#'
#' In the package WoodGradeR, a number of predefined base values for simulation
#' are stored -- see \code{\link{simbase}}.
#'
#' Using a character vector for the argument \code{subsets} leads to subsets
#' as equal in size as possible.
#'
#' The argument \code{subsets} enables differing means and standard deviations
#' for different subsamples. There are several possible usages:
#' \itemize{
#'  \item If \code{subsets = NULL}, the information about means and standard
#'    deviations is taken from the \code{simbase}. There can still be different
#'    means and standard deviations if \code{simbase} is an object of class
#'    \code{\link{simbase_list}}.
#'  \item If a numeric vector or a character vector, it is used as argument
#'    \code{country} in an internal call to \code{\link{get_subsample_definitions}}.
#'  \item If a dataset, it has to have columns \code{country} and
#'    \code{subsample}, the latter being unique per row.
#'    A column \code{share} can be used to create subsamples of different sizes
#'    proportional to the values in \code{share}.
#'    For a least one of the variables defined in the \code{simbase}, also the
#'    mean \emph{and} the standard deviation need to be given in each row; the
#'    column names for this data must be the name of the respective variable(s)
#'    from the \code{simbase}, suffixed by \code{_mean} and \code{_sd},
#'    respectively.
#' }
#'
#' The argument simbase can be either an object of class
#' \code{\link{simbase_covar}} or of class \code{\link{simbase_list}}.
#' \itemize{
#'  \item various predefined \code{\link{simbase_covar}} objects are available
#'    in \code{WoodSimulatR} -- see \code{\link{simbase}}.
#'  \item for objects of class \code{\link{simbase_list}}, additional
#'    restrictions apply: the object may only have the grouping variable(s)
#'    "country" and/or "subsample", and the value combinations in
#'    "country"/"subsample" have to match those given in the \code{subsets}
#'    argument respectively in the subsample definitions this \code{subsets}
#'    argument leads to (see also \code{\link{get_subsample_definitions}}).
#' }
#'
#' Both the means and standard deviations in the subsample definitions
#' (see \code{\link{get_subsample_definitions}}) as well as the values in the
#' `simbase` depend on the way the destructive testing of the sawn timber was
#' done. If the `simbase` has a field `loadtype`
#' (see also \code{\link{simbase_covar}}), this value is used in the call to
#' \code{\link{get_subsample_definitions}}. Otherwise, the `loadtype` has to be
#' passed directly to the present function unless no call to
#' \code{\link{get_subsample_definitions}} is necessary (this depends on the
#' value of `subsets` -- see above). If a loadtype has been defined, a variable
#' `loadtype` is also created in the resulting dataset for reference.
#'
#' Negative values in any numeric column of the result dataset are forced to
#' zero.
#'
#' If \code{random_seed} is not \code{NULL}, reproducibility of results
#' is enforced by using \code{\link{set.seed}} with arguments
#' \code{kind='Mersenne-Twister'} and \code{normal.kind='Inversion'},
#' and by calling \code{\link{RNGversion}} with argument \code{'3.5.0'}.
#'
#' If \code{random_seed} is not \code{NULL}, the random number generator
#' is reset at the end of the function using \code{set.seed(NULL)} and
#' \code{RNGversion(toString(getRversion()))}.
#'
#' @param n Number of rows in the dataset
#' @param subsets Either \code{NULL},
#'  or a \code{data.frame} describing the subsets (see
#'  details) or a character vector or named numeric vector suitable for
#'  argument \code{country} in \code{\link{get_subsample_definitions}}.
# @param variables A string vector of variables to return. Allowed values are
#   \code{'f', 'E', 'rho', 'ip_edyn_u', 'ip_f', 'ip_edyn', 'ip_rho'}. \cr
#   If an entry of the string vector is named, the respective variable is
#   renamed to that value (similar to \code{\link{rename}}). \cr
#   \bold{Not yet implemented!}
#' @param random_seed Allows to set an integer seed value for the random number
#'  generator to achieve reproducible results
#'  (see also \code{\link{set.seed}}).
#' @param simbase An object of class \code{\link{simbase_covar}} or
#'  \code{\link{simbase_list}}. In particular, one of the simbases stored in
#'  \code{WoodSimulatR} may be used -- see \code{\link{simbase}}.
#' @param loadtype For passing on to \code{\link{get_subsample_definitions}}.
#'  A string with either "t" (for material tested in tension) or "be" (for
#'  material tested in edgewise bending). Is only used if the simbase doesn't
#'  contain a field \code{loadtype} or if the loadtype is ambiguous or not
#'  equal to "t" or "be".
#' @param ... arguments passed on to \code{\link{get_subsample_definitions}}.
#'
#' @importFrom tibble add_column
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @importFrom stats sd
#'
#' @export
simulate_dataset <- function(n=5000, subsets=4, random_seed=NULL,
                             simbase=WoodSimulatR::ws_t_logf, loadtype = NULL, ...) {
  stopifnot(length(n) == 1);
  n <- round(n);     # force n to be an integer value # XXX give a warning if not integer?
  stopifnot(n > 0);

  #if (!is.null(variables)) stop('variables selection/renaming is not (yet) implemented!')

  # reproducible random number generation
  if (!is.null(random_seed)) {
    #message('random number generator with fixed seed')
    RNGversion('3.5.0');
    set.seed(random_seed, kind='Mersenne-Twister', normal.kind='Inversion');

    # and make sure that random number generation is reset even in the case of
    # an error!
    on.exit({
      #message('resetting the random number generator')
      RNGversion(toString(getRversion()));
      set.seed(NULL);
    });
  }

  # exta checks if we have a simbase_list
  if (inherits(simbase, 'simbase_list')) {
    simbase_groups <- setdiff(names(simbase), '.simbase');
    j <- setdiff(simbase_groups, c('country', 'subsample'));
    if (length(j) > 0) {
      stop('only the grouping variables country or subsample are allowed in simbase_list! Found ',
           paste(j, collapse = ', '));
    }

    j <- c('country', 'subsample') %in% simbase_groups;
    if (!any(j)) {
      stop('no grouping variables in the simbase_list???');
    }
  }

  # get loadtype from the simbase. We do this in any case so we can add a
  # `loadtype` variable to the dataset.
  # XXX this should perhaps be moved to an S3 method for more flexibility?
  #   On the other hand, simulate_dataset only works with these two types of
  #   simbase at the moment.
  #   So maybe, we'd rather have to turn simulate_dataset into an S3 method?
  lt_simbase <- switch(
    class(simbase)[1],
    'simbase_covar' = simbase$loadtype,
    'simbase_list' = unique(unlist(purrr::map(simbase$.simbase, ~ .x$loadtype))),
    stop('unsupported class for simbase!')
  );
  if (length(lt_simbase) == 0) {
    if (!is.null(loadtype)) {
      lt_simbase <- loadtype;
    }
  } else if (length(lt_simbase) == 1 && lt_simbase %in% c('t', 'be')) {
    if (!is.null(loadtype) && loadtype != lt_simbase) {
      warning('using loadtype = ', lt_simbase, ' from the simbase!');
    }
  } else {
    if (is.null(loadtype)) {
      stop('ambiguous values for loadtype from the simbase, set loadtype manually as an argument to simulate_dataset!');
    }
    warning('ambiguous values for loadtype from the simbase, using loadtype=', loadtype);
    lt_simbase <- loadtype;
  }

  # process the subsets argument
  subset_null <- is.null(subsets);
  if (subset_null) {
    if (inherits(simbase, 'simbase_list')) {
      subsets <- dplyr::select(simbase, -.data$.simbase);
      j <- c('country', 'subsample') %in% names(subsets);
      if (!any(j)) {
        stop('no grouping variables in the simbase_list???');
      } else if (j[1] && !j[2]) {
        subsets <- dplyr::mutate(subsets, subsample = make.unique(.data$country, sep = '_'));
      } else if (!j[1] && j[2]) {
        subsets <- dplyr::mutate(subsets, country = .data$subsample);
      }

      subsets <- tibble::add_column(subsets, share = 1);

    } else if (inherits(simbase, 'simbase_covar')) {
      subsets <- tibble::tibble(country = 'C1', 'subsample' = 'C1', share = 1);

    } else stop('simbase type not supported!');

  } else if (is.data.frame(subsets)) {
    stopifnot(nrow(subsets) >= 1);
    stopifnot(all(c('country', 'subsample') %in% names(subsets)));
    if (length(unique(subsets$subsample)) != nrow(subsets)) {
      stop('The entries in subsets$subsample are not unique per row!')
    }
    if (!('share' %in% names(subsets))) {
      subsets <- tibble::add_column(subsets, share = 1);
    }
    # TODO: more checks

  } else {

    if (length(lt_simbase) == 0) {
      if (is.null(loadtype)) {
        stop('no value for loadtype stored in the simbase, set loadtype manually as an argument to simulate_dataset!');
      }
      lt_simbase <- loadtype;
    }

    subsets <- get_subsample_definitions(country = subsets, loadtype = lt_simbase, ...);
  }

  # process the simbase argument
  stopifnot(rlang::inherits_any(simbase, c('simbase_covar', 'simbase_list')));

  if (inherits(simbase, 'simbase_list')) {
    simbase_variables_list <- purrr::map(simbase$.simbase, ~.x$variables);
    simbase_variables <- simbase_variables_list[[1]];

    # check that we have the same variables in each sub-simbase
    j <- purrr::map_lgl(simbase_variables_list, ~ setequal(.x, simbase_variables));
    if (!all(j)) {
      stop('In the simbase_list object, some of the sub-simbases have a different variables list!')
    }

    # match the sub-simbases to the subsets
    stopifnot(!('.simbase' %in% names(subsets)));
    subsets <- dplyr::left_join(subsets, simbase, by = simbase_groups);

    j <- is.na(subsets$.simbase);
    if (any(j)) {
      warning('for some rows of "subsets", no fitting row of the simbase was found! They are removed.');
      subsets <- dplyr::filter(subsets, !is.na(.data$.simbase));
    }

  } else if (inherits(simbase, 'simbase_covar')) {
    simbase_variables <- simbase$variables;
    simbase_current <- simbase;
  }

  if (subset_null) {
    predef_vars <- simbase_variables;
    further_vars <- c();

  } else {
    # identify which variables have predefined mean and standard deviation in
    # "subsets"
    predef <- purrr::map_dbl(
      simbase_variables,
      ~ 1*(paste0(.x, '_mean') %in% names(subsets)) + 2*(paste0(.x, '_sd') %in% names(subsets))
    );
    i <- predef == 3;
    if (!any(i)) {
      stop('no mean and sd definitions in subsets for any of the variables found in simbase!');
    }

    predef_vars <- simbase_variables[i];
    further_vars <- simbase_variables[!i];
    positive_sd <- purrr::map_lgl(
      subsets[, paste0(predef_vars, '_sd')],
      ~ all(.x > 0)
    );
    if (any(!positive_sd)) {
      stop('the subsets columns ', paste0(predef_vars, '_sd', collapse = ', '), ' are not all positive!');
    }
  }

  # calculate the number of pieces per subset
  subsets$n <- diff(round(n * cumsum(c(0, subsets$share)) / sum(subsets$share)));

  # create dataset subset by subset
  ds <- tibble::tibble();
  for (i in 1 : nrow(subsets)) {
    # get current simbase (if simbase_list)
    if (inherits(simbase, 'simbase_list')) {
      simbase_current <- subsets$.simbase[[i]];
    }

    # first stage of simulation: simulate the predef variables and set the means
    # and standard deviations accordingly.
    predef_covar <- simbase_current$covar[predef_vars, predef_vars];

    # Cholesky factorization as preparation for generating correlated random
    # normal covariates
    sim_chol <- chol(predef_covar);

    # Generate correlated random normal covariates
    sim_rnorm <- matrix(
      stats::rnorm(subsets$n[i] * ncol(predef_covar)),
      nrow = subsets$n[i],
      ncol = ncol(predef_covar),
      dimnames = list(NULL, colnames(predef_covar))) %*% sim_chol;

    # Add mean values -- otherwise, the variables would all be centered around 0
    predef_mean <- simbase_current$means[predef_vars];
    sim_r <- sim_rnorm + (matrix(1, nrow=subsets$n[i], ncol=1) %*% predef_mean);

    # convert to tibble and add the subsample and country information
    sim_r <- tibble::as_tibble(sim_r);
    sim_r <- tibble::add_column(
      sim_r,
      country = subsets$country[i],
      subsample = subsets$subsample[i]
    );

    if (subset_null) {
      # execute the back-transforms
      for (v in intersect(predef_vars, names(simbase_current$transforms))) {
        sim_r[[v]] <- do.call(simbase_current$transforms[[v]]$inverse, list(sim_r[[v]]));
      }

    } else {
      # execute the back-transforms and adjust values to get the means and
      # standard deviations defined in "subsets" -- this has to be done differently
      # for transformed variables
      # - get standard deviation from covariance matrix
      predef_sd <- sqrt(diag(predef_covar));
      # - get target standard deviation from "subsets"
      target_sd <- structure(subsets[i, paste0(predef_vars, '_sd')], names = predef_vars);
      # - get target means from "subsets"
      target_mean <- structure(subsets[i, paste0(predef_vars, '_mean')], names = predef_vars);
      # - back-transforms
      for (v in intersect(predef_vars, names(simbase_current$transforms))) {
        sim_r[[v]] <- do.call(simbase_current$transforms[[v]]$inverse, list(sim_r[[v]]));
        # adjust mean and standard deviation -- XXX this leads to exact matches!
        sim_r[[v]] <- (sim_r[[v]] - mean(sim_r[[v]], na.rm = TRUE)) /
          stats::sd(sim_r[[v]], na.rm = TRUE) * target_sd[[v]] + target_mean[[v]];
      }
      # - adjust means and standard deviations for non-transformed variables
      for (v in setdiff(predef_vars, names(simbase_current$transforms))) {
        sim_r[[v]] <- (sim_r[[v]] - predef_mean[v]) /
          predef_sd[v] * target_sd[[v]] + target_mean[[v]];
      }
    }

    # negative values should be set to zero
    # XXX do we really want this to happen to all numeric columns?
    #  -> probably yes...
    sim_r <- dplyr::mutate_at(sim_r, .vars = predef_vars, .funs = ~pmax(0, .x));

    # conditionally add the remaining variables (if any)
    if (length(further_vars) > 0) {
      sim_r <- simulate_conditionally(sim_r, simbase_current);
    }

    # bind to full dataset
    ds <- dplyr::bind_rows(ds, sim_r);
  }

  # add loadtype variable to dataset if available
  if (length(lt_simbase) > 0) {
    ds <- tibble::add_column(ds, loadtype = lt_simbase)
  }

  ds;
}
