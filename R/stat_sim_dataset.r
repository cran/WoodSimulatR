#' Generate an artificial dataset with correlated variables
#'
#' Generate an artificial dataset with correlated variables and defined means
#' and standard deviations.
#'
#' In the package WoodSimulatR, a number of predefined base values for simulation
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
#'  \item If a dataset, there are the following requirements:
#'    \itemize{
#'      \item \emph{identifier columns}: The dataset has to have one or more
#'        discrete-valued \emph{identifier columns} (usually character vectors or
#'        factors) which uniquely identify each row.
#'        These \emph{identifier columns} are named \code{"country"} and
#'        \code{"subsample"} in the standard case as yielded by
#'        \code{\link{get_subsample_definitions}}.
#'        In the general case, the identifier columns are detected as those
#'        columns which are not named \code{share, species, loadtype} or
#'        \code{literature} and which do not end in \code{_mean} or \code{_sd}.
#'        If the argument \code{simbase} is of class \code{\link{simbase_list}},
#'        further restrictions apply (see below).
#'      \item \emph{means and standard deviations}: For at least one of the
#'        variables defined in the \code{simbase}, also the mean \emph{and} the
#'        standard deviation need to be given in each row; the column names for
#'        this data must be the name of the respective variable(s)
#'        from the \code{simbase}, suffixed by \code{_mean} and \code{_sd},
#'        respectively.
#'      \item \emph{optional}: A column \code{share} can be used to create
#'        subsamples of different sizes proportional to the values in
#'        \code{share}.
#'    }
#' }
#'
#' The argument \code{simbase} can be either an object of class
#' \code{\link{simbase_covar}} or of class \code{\link{simbase_list}}.
#' \itemize{
#'  \item various predefined \code{\link{simbase_covar}} objects are available
#'    in \code{WoodSimulatR} -- see \code{\link{simbase}}.
#'  \item for objects of class \code{\link{simbase_list}}, additional
#'    restrictions apply:
#'    \enumerate{
#'      \item the object may only have grouping variable(s) which are also
#'        \emph{identifier columns} according to the \code{subsets} definition
#'        above -- if the \code{subsets} argument is \emph{not} a data frame,
#'        the \emph{identifier columns} are "country" and "subsample".
#'      \item The value combinations in the \emph{identifier columns} have to
#'        match those which the \code{subsets} argument leads to
#'        (see also \code{\link{get_subsample_definitions}}).
#'    }
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
#' and by calling \code{\link{RNGversion}} with argument \code{RNGversion}.
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
#' @param RNGversion In \code{WoodSimulatR 0.5}, the \code{RNGversion} had been
#'  fixed to \code{RNGversion = "3.5.0"}, but this setting now causes a warning
#'  because the random number generator was changed in R version 3.6.0
#'  (see \code{\link{RNGversion}}).
#'  For perfect reproducibility of results from \code{WoodSimulatR 0.5},
#'  one should set \code{RNGversion = "3.5.0"}.
#'
#' @importFrom dplyr across bind_rows filter group_by left_join mutate select summarise
#' @importFrom purrr map map_dbl map_lgl
#' @importFrom rlang inherits_any syms
#' @importFrom stats rnorm runif sd
#' @importFrom tibble add_column as_tibble tibble
#' @importFrom tidyselect all_of
#'
#' @export
#'
#' @examples
#' simulate_dataset(n = 10, subsets = 1, random_seed = 1)
#'
#' # As the loadtype is defined in the simbase, the argument loadtype is ignored
#' # with a warning
#' simulate_dataset(n = 10, subsets = 1, random_seed = 1, loadtype = 'be')
#'
#' # Two subsamples
#' simulate_dataset(n = 10, subsets = 2, random_seed = 1)
#'
#' # Two subsamples from pre-defined countries
#' simulate_dataset(n = 10, subsets = c('at', 'de'), random_seed = 1)
#'
#' # Two subsamples from pre-defined countries with different sample sizes
#' simulate_dataset(n = 10, subsets = c(at = 3, de = 2), random_seed = 1)
simulate_dataset <- function(n=5000, subsets=4, random_seed=NULL,
                             simbase=WoodSimulatR::ws_t_logf, loadtype = NULL,
                             ..., RNGversion = '3.6.0') {
  stopifnot(length(n) == 1);
  n <- round(n);     # force n to be an integer value # XXX give a warning if not integer?
  stopifnot(n > 0);

  #if (!is.null(variables)) stop('variables selection/renaming is not (yet) implemented!')

  # reproducible random number generation
  if (!is.null(random_seed)) {
    #message('random number generator with fixed seed')
    RNGversion(RNGversion);
    set.seed(random_seed, kind='Mersenne-Twister', normal.kind='Inversion');

    # and make sure that random number generation is reset even in the case of
    # an error!
    on.exit(add = TRUE, after = FALSE, {
      #message('resetting the random number generator')
      RNGversion(toString(getRversion()));
      set.seed(NULL);
    });
  }

  # Get the identifier_columns
  # They are needed here already so that we can do the necessary checks on
  # simbase_list, if the latter is present
  subset_null <- is.null(subsets);
  if (!subset_null && is.data.frame(subsets)) {
    # certain names cannot be identifier_columns (see help)
    identifier_columns <- setdiff(
      names(subsets),
      c('share', 'species', 'loadtype', 'literature')
    );

    # certain suffixes are not allowed for identifier_columns
    identifier_columns <- identifier_columns[!endsWith(identifier_columns, '_mean')];
    identifier_columns <- identifier_columns[!endsWith(identifier_columns, '_sd')];

    if (length(identifier_columns) == 0) {
      stop('no identifier columns found in parameter `subsets`!');
    }

  } else {
    identifier_columns <- c('country', 'subsample');
  }

  # extra checks if we have a simbase_list
  if (inherits(simbase, 'simbase_list')) {
    simbase_groups <- setdiff(names(simbase), '.simbase');
    j <- setdiff(simbase_groups, identifier_columns);
    if (length(j) > 0) {
      stop('only the grouping variables ',
           paste(identifier_columns, collapse = ', '),
           ' are allowed in the `simbase_list` object! Found ',
           paste(simbase_groups, collapse = ', '));
    }

    j <- identifier_columns %in% simbase_groups;
    if (!any(j)) {
      stop('unexpected: no identifier columns in the `simbase_list` object???');
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
    stop('unsupported class for `simbase`!')
  );
  if (length(lt_simbase) == 0) {
    if (!is.null(loadtype)) {
      lt_simbase <- loadtype;
    }
  } else if (length(lt_simbase) == 1 && lt_simbase %in% c('t', 'be')) {
    if (!is.null(loadtype) && loadtype != lt_simbase) {
      warning('using loadtype = ', lt_simbase, ' from the `simbase`!');
    }
  } else {
    if (is.null(loadtype)) {
      stop('ambiguous values for loadtype from the `simbase`, set loadtype manually as an argument to `simulate_dataset`!');
    }
    warning('ambiguous values for loadtype from the `simbase`, using loadtype=', loadtype);
    lt_simbase <- loadtype;
  }

  # process the subsets argument
  if (subset_null) {
    if (inherits(simbase, 'simbase_list')) {
      subsets <- dplyr::select(simbase, -.data$.simbase);
      # ATTENTION: in the subsets_null case, the identifier_columns are
      # ALWAYS country and subsample!!!
      # TODO: add a test to pin this down?
      j <- identifier_columns %in% names(subsets);
      if (!any(j)) {
        stop('no grouping variables in the `simbase_list` object???');
      } else if (j[1] && !j[2]) {
        subsets <- dplyr::mutate(subsets, subsample = make.unique(.data$country, sep = '_'));
      } else if (!j[1] && j[2]) {
        subsets <- dplyr::mutate(subsets, country = .data$subsample);
      }

      subsets <- tibble::add_column(subsets, share = 1);

    } else if (inherits(simbase, 'simbase_covar')) {
      # ATTENTION: in the subsets_null case, the identifier_columns are
      # ALWAYS country and subsample!!!
      subsets <- tibble::tibble(country = 'C1', 'subsample' = 'C1', share = 1);

    } else stop('`simbase` class not supported!');

  } else if (is.data.frame(subsets)) {
    stopifnot(nrow(subsets) >= 1);

    # check for uniqueness
    j <- dplyr::summarise(
      dplyr::group_by(subsets, !!! rlang::syms(identifier_columns)),
      n = dplyr::n(),
      .groups = 'drop'
    );
    if (any(j$n > 1) || nrow(j) != nrow(subsets)) {
      stop('The values in the `subsets` identifier columns are not unique per row!')
    }

    # add share column if it is missing
    if (!('share' %in% names(subsets))) {
      subsets <- tibble::add_column(subsets, share = 1);
    }
    # TODO: more checks

  } else {

    if (length(lt_simbase) == 0) {
      if (is.null(loadtype)) {
        stop('no value for loadtype stored in the `simbase`, set loadtype manually as an argument to `simulate_dataset`!');
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
      stop('In the `simbase_list` object, some of the sub-simbases have a different variables list!')
    }

    # match the sub-simbases to the subsets
    stopifnot(!('.simbase' %in% names(subsets)));
    subsets <- dplyr::left_join(subsets, simbase, by = simbase_groups);

    j <- is.na(subsets$.simbase);
    if (any(j)) {
      warning('for some rows of `subsets`, no fitting row of the `simbase` was found! They are removed.');
      subsets <- dplyr::filter(subsets, !is.na(.data$.simbase));
    }

  } else if (inherits(simbase, 'simbase_covar')) {
    simbase_variables <- simbase$variables;
    simbase_current <- simbase;

  } else stop('`simbase` class not supported!');

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
    i <- predef > 0 & predef != 3;
    if (any(i)) {
      warning(
        'for the following variables found in `simbase` and mentioned in `subsets`, ',
        'either the mean or the sd definition is missing in `subsets`: ',
        paste(simbase_variables[i], collapse = ', '), '. ',
        'These variables will be treated as though they were not mentioned in `subsets`!'
      );
    }
    i <- predef == 3;
    if (!any(i)) {
      stop('no mean and sd definitions in `subsets` for any of the variables found in `simbase`!');
    }

    predef_vars <- simbase_variables[i];
    further_vars <- simbase_variables[!i];
    positive_sd <- purrr::map_lgl(
      subsets[, paste0(predef_vars, '_sd')],
      ~ all(.x > 0)
    );
    if (any(!positive_sd)) {
      stop('the `subsets` columns ',
           paste0(predef_vars[!positive_sd], '_sd', collapse = ', '),
           ' contain non-positive values!'
      );
    }
  }

  # check that there is no intersection between simbase_variables and
  # identifier_columns -- both will be present in the final simulated data set
  # and must therefore have different names
  i <- intersect(simbase_variables, identifier_columns);
  if (length(i) > 0) {
    stop('The following variable names are used both for identifier columns ',
         'and for simulated variables in the `simbase`: ',
         paste(i, collapse = ', '), ' -- this is not allowed!'
    );
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
    predef_covar <- simbase_current$covar[predef_vars, predef_vars, drop = FALSE];

    # Cholesky factorization as preparation for generating correlated random
    # normal covariates
    sim_chol <- chol(predef_covar);

    # For subsets$n[i] == 1, we get errors with forcing the standard deviation
    # for transformed variables.
    # To be on the safe side, we initially create more rows if transformed
    # variables with predefined mean and standard deviation exist.
    # NOTE: this could also be a trick to make mean and standard deviation of
    # transformed variables differ from the forced target values, but the
    # question then is: How many more rows to simulate?
    if (subsets$n[i] == 1 && any(predef_vars %in% names(simbase_current$transforms))) {
      n_to_simulate <- 2;
    } else {
      n_to_simulate <- subsets$n[i];
    }

    # Generate correlated random normal covariates
    sim_rnorm <- matrix(
      stats::rnorm(n_to_simulate * ncol(predef_covar)),
      nrow = n_to_simulate,
      ncol = ncol(predef_covar),
      dimnames = list(NULL, colnames(predef_covar))) %*% sim_chol;

    # Add mean values -- otherwise, the variables would all be centred around 0
    predef_mean <- simbase_current$means[predef_vars];
    sim_r <- sim_rnorm + (matrix(1, nrow = n_to_simulate, ncol = 1) %*% predef_mean);

    # convert to tibble and add the identifier_columns information
    sim_r <- tibble::as_tibble(sim_r);
    sim_r <- tibble::add_column(sim_r, !!! subsets[i, identifier_columns, drop = FALSE], .before = 1);

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

    # If n_to_simulate deviates from subsets$n[i], we correct the number of rows
    # here.
    if (n_to_simulate != subsets$n[i]) {
      sim_r <- sim_r[1 : subsets$n[i], ];
    }

    # negative values should be set to zero
    # XXX do we really want this to happen to all numeric columns?
    #  -> probably yes...
    sim_r <- dplyr::mutate(
      sim_r,
      dplyr::across(.cols = tidyselect::all_of(predef_vars), .fns = ~pmax(0, .x))
    );

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
