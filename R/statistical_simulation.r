#' Wrapper for the \code{simbase_*} functions for grouped data
#'
#' If a function of the \code{simbase_*} family encounters grouped \code{data}
#' (as caused by [dplyr::group_by()]), it should invoke \code{simbase_list}
#' to create a collection of separate simbases for each group.
#'
#' @section Technical details:
#' Currently, the "\code{simbase_*} family" only consists of
#' [simbase_covar()] (although, in a broader sense,
#' \code{simbase_list} can also be thought to be part of this "family").
#' It is planned to add further simulation types in a later release.
#'
#' The functions of the \code{simbase_*} family support label
#' generation (see e.g. [simbase_covar()]). These functions should
#' generate the label \emph{before} invoking \code{simbase_list}, so that there
#' is a common label for all of the simbases; \code{simbase_list} adds a suffix
#' \code{suffix}. A warning is issued if the labels of the different simbases
#' are not all equal; no suffix is added in this case.
#'
#' @param data A grouped dataset (see [dplyr::group_by()])
#' @param simbase_constructor A function which returns a \code{simbase_*}
#'  object, like [simbase_covar()]
#' @param \dots Further arguments passed to the \code{simbase_*} function.
#' @param suffix Suffix to be added to the individual simbase labels if they are
#'  all the same (see details).
#'
#' @return A \code{simbase_list} object; this is essentially a
#'  [tibble::tibble] with the grouping columns of \code{data} and a column
#'  \code{.simbase} which contains the \code{simbase_*} objects.
#'
#' @importFrom dplyr group_nest
#' @importFrom dplyr group_vars
#' @importFrom dplyr is_grouped_df
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom rlang .data
#'
#' @export
simbase_list <- function(data, simbase_constructor, ..., suffix = '_lst') {
  .simbase <- NULL; # due to NSE notes in R CMD check

  stopifnot(dplyr::is_grouped_df(data));
  if ('.simbase' %in% dplyr::group_vars(data)) {
    stop('As ".simbase" is the column where the simbases are stored, there mustn\'t be a grouping variable of this name!');
  }

  # create the simbase objects
  simbase <- dplyr::group_nest(data, .key = '.simbase');
  class(simbase) <- c('simbase_list', class(simbase));
  simbase <- dplyr::mutate(
    simbase,
    .simbase = purrr::map(
      .simbase,
      simbase_constructor,
      ...
    )
  );

  # check that the simbase labels ar eall the same, and add the suffix "_lst"
  # in this case.
  # (programmer's note: the first entry in a vector can never return TRUE for
  # duplicated, so it has to be removed -- indexing with [-1] -- before we check
  # that all are duplicated...)
  lbls <- purrr::map_chr(simbase$.simbase, ~ .x$label);
  if (all(duplicated(lbls)[-1])) {
    simbase <- dplyr::mutate(
      simbase,
      .simbase = purrr::map(
        .simbase,
        function(sb) { sb$label <- paste0(sb$label, suffix); sb; }
      )
    )
  } else {
    warning('The simbases have different labels: ',
            paste(sort(unique(lbls)), collapse = ', '), '!');
  }

  # return the simbase_list object
  simbase;
}

#' Calculate reference data for simulating values based on a covariance matrix approach
#'
#' Given the covariance matrix and the means of a set of variables, we can
#' simulate not only the distribution of the variables, but also their
#' correlations. The present function calculates the basic values required for
#' the simulation and returns them packed into an object of class
#' `simbase_covar`.
#'
#' If some of the variables are non-normally distributed, a transform may
#' improve the prediction. The transforms are passed to the function as a named
#' list, where the name of a list entry must correspond to the name of the
#' variable in the data which is to be transformed.
#'
#' Predefined transforms can be found in the package `scales`, where they are
#' used for axis transformations as a preparation for plotting. The package
#' `scales` also contains a function \code{trans_new} which can be used
#' to define new transforms.
#'
#' In the context of destructively measured sawn timber properties, the type of
#' destructive test applied is of interest. If the dataset `data` contains a
#' variable `loadtype` which consistently throughout the dataset has either the
#' value "t" (i.e. all sawn timber has been tested in tension) or the
#' value "be" (i.e. all sawn timber has been tested in bending, edgewise),
#' then the returned object also has a field `loadtype` with that value.
#'
#' One can also calculate a simbase under the assumption that the correlations
#' are different for different subgroups of the data. This is done by grouping
#' the dataset `data` prior to passing it to the function,
#' using [dplyr::group_by()]. In this case, several objects of
#' class `simbase_covar` are created and joined together in a [tibble::tibble] --
#' see also [simbase_list()].
#'
#' @param data The dataset for the calculation of the reference data for
#'  simulation; for grouped datasets (see [dplyr::group_by()]),
#'  the reference data is
#'  calculated for each group separately (see also [simbase_list()]).
#' @param variables Character vector containing the names in \code{data}
#'   which should be included in the simulation. If missing, all numeric
#'   variables in \code{data} are used.
#' @param transforms A named list of objects of class \code{trans}
#'   or class \code{transform}
#'   (see function \code{trans_new} in package \code{scales});
#'   the name of each list entry
#'   \bold{must} correspond to a variable name in \code{variables}.
#' @param label Either a string describing the data and the simulation approach,
#'    or a labelling function which returns a label string and takes as input
#'    the data, a string giving the class
#'    of the simbase object (here \code{"simbase_covar"}) and the
#'    transforms list.
#' @param ... Arguments to be passed on to [simbase_list()]
#'  (\emph{if} it is called).
#'
#' @return An \code{S3} object of class `simbase_list` if `data` is grouped,
#'  and an object of class \code{simbase_covar} otherwise.
#'
#' @importFrom dplyr is.grouped_df
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom purrr map_dbl
#' @importFrom purrr map_lgl
#' @importFrom rlang syms
#' @importFrom stats cov
#'
#' @examples
#' # obtain a dataset for demonstration
#' dataset <- simulate_dataset();
#'
#' # calculate a simbase without transforms
#' simbase_covar(dataset, c('f', 'E', 'rho', 'E_dyn'));
#'
#' # calculate a simbase with log-transformed f
#' simbase_covar(dataset, c('f', 'E', 'rho', 'E_dyn'), list(f = scales::log_trans()));
#'
#' # if we group the dataset, we get a simbase_list object
#' simbase_covar(dplyr::group_by(dataset, country), c('f', 'E', 'rho', 'E_dyn'));
#'
#' @export
simbase_covar <- function(data, variables = NULL, transforms=list(), label = simbase_labeler, ...) {
  if (is.null(variables)) {
    variables <- names(data)[
      purrr::map_lgl(data, is.numeric)
    ];
  }

  # check validity of parameter values
  # variables is a character vector
  stopifnot(is.character(variables))
  variables <- as.character(variables); # necessary to strip any attributes, in particular names, which interfere with dplyr::select() and may cause unwanted variable renames
  # variables exist in data
  stopifnot(variables %in% names(data))

  # all names of transforms list entries appear in variables
  # is transforms a list?
  if (is.null(transforms)) transforms <- list();
  stopifnot(is.list(transforms))
  if (length(transforms) > 0) {
    # do the transforms have names?
    stopifnot(!is.null(names(transforms)));
    # are the names of the list entries of transforms contained in variables?
    stopifnot(all(names(transforms) %in% variables))
    # are all list entries objects of class "trans" or "transform"?
    i <- purrr::map_lgl(transforms, ~ inherits(.x, c('trans', 'transform')));
    if (any(!i)) {
      stop('All transforms must be objects of class "trans" or class "transform" (see scales::trans)')
    }
  }

  # get label
  if (is.character(label)) {
    stopifnot(length(label) == 1)
  } else if (is.function(label)) {
    label <- label(data, 'simbase_covar', transforms)
  } else stop('label must either be a string or a labeler function, but is a ', class(label))

  if (dplyr::is_grouped_df(data)) {
    return(
      simbase_list(
        data = data,
        simbase_constructor = simbase_covar,
        variables = variables,
        transforms = transforms,
        label = label,
        ...
      )
    );
  }

  # construct results list
  result <- list(
    label = label,
    variables = variables,
    transforms = transforms,
    covar = matrix(NA, length(variables), length(variables)),
    means = rep(NA, length(variables))
  );

  # Add field loadtype if the data contain that variable, and is either always
  # 't' or always 'be'.
  if ('loadtype' %in% names(data)) {
    lt <- unique(data$loadtype);
    if (length(lt) == 1 && lt %in% c('be', 't')) {
      result$loadtype <- lt;
    }
  }


  # execute the transforms
  for (i in names(transforms)) {
    data[, i] <- do.call(transforms[[i]]$transform, list(data[, i]));
  }

  # reduce the dataset to the required variables
  # grouping variables cannot be removed; therefore, we remove any grouping first.
  if (dplyr::is.grouped_df(data)) data <- dplyr::ungroup(data);
  data <- dplyr::select(data, !!! rlang::syms(variables));

  # calculate covariance matrix and means
  result$covar <- stats::cov(data);
  result$means <- purrr::map_dbl(data, mean);

  # set class attribute
  class(result) <- 'simbase_covar';

  result
}

#' Default labelling function for simbase objects
#'
#' Each simbase object should have a label which can be used for differentiating
#' different simulations. This function tries to simplify the label generation.
#'
#' Primarily, this function is intended to be called as a default from
#' [simbase_covar()]. It can also serve as a template for creating
#' custom labelling functions.
#'
#' @param data The dataset for the calculation of the basic simulation data.
#' @param simbase_class The class of the simbase object for which the label is
#'    to be generated. Currently, only \code{"simbase_covar"} is supported.
#' @param transforms The transforms applied to variables in the dataset.
#'  Must be objects of class \code{trans} or class \code{transform}
#'  (see function \code{trans_new} in package \code{scales}).
#'
#' @return A string for labelling a simbase object.
#'
#' @importFrom rlang is_primitive
#' @importFrom rlang prim_name
#'
#' @export
simbase_labeler <- function(data, simbase_class, transforms) {
  stopifnot(is.data.frame(data))
  stopifnot(nrow(data) > 0)
  stopifnot(simbase_class %in% c('simbase_covar'));

  l <- sprintf('n%i', nrow(data));

  if ('loadtype' %in% names(data)) {
    lt <- unique(data$loadtype);
    if (length(lt) == 1 && lt %in% c('be', 't')) {
      l <- paste0(l, '_', lt);
    }
  }

  l <- paste0(l, '_', switch(
    simbase_class,
    "simbase_covar"       = 'cov'
  )
  );
  if (length(transforms) > 0) {
    fnames <- get_transform_names(transforms);
    fnames <- paste(fnames, names(fnames), collapse = '_', sep = '');
    l <- paste0(l, '_', fnames);
  }

  l
}

#' Return labels for given transforms
#'
#' The function [simbase_covar()] allows the specification of a
#' transform for one or more variables. The present function creates short
#' names for such transforms for use in labelling (by default, the labelling is
#' done by [simbase_labeler()]).
#'
#' The label of a transform could be the value of the field \code{name} from
#' each object of class \code{trans} (or \code{transform}),
#' but also the name of the transform
#' function itself, if it is a primitive function or just calls one function.
#'
#' Each object of class \code{trans} (or \code{transform})
#' should have a field \code{name}
#' which can be returned by the present function.
#'
#' The function examines the field \code{transform}.
#' If this field contains a primitive function (see [rlang::is_primitive()]),
#' or if there is just one function call in the body of this \code{transform}
#' function, we can also return the name of this called function.
#'
#' If there is no field \code{name} and no single function is called from the
#' function defined in the field \code{transform},
#' a generic function name \code{"f."} is returned.
#'
#' @param transforms A named list of objects of class \code{trans}
#'  or class \code{transform}
#'  (see function \code{trans_new} in package \code{scales})
#' @param prefer_primitive If "never", the function always returns the value of
#'  the field \code{name} (except if this is missing).
#'  If "always", the name of the called function is returned unless it cannot be
#'  identified (in many cases, the transform will not be primitive).
#'  If "if_shorter", the shorter option of the two above is returned if both can
#'  be retrieved.
#'
#'
#' @return A named vector of transforms names.
#' @export
#'
#' @examples
#' get_transform_names(list(a = scales::log_trans(), b = scales::boxcox_trans(0)));
#' get_transform_names(list(x = list(name = 'a very long name', transform = log, inverse = exp)))
get_transform_names <- function(transforms, prefer_primitive = c('if_shorter', 'never', 'always')) {
  fname <- character();
  prefer_primitive <- match.arg(prefer_primitive);

  if (is.list(transforms) && length(transforms) > 0) {
    for (i in 1 : length(transforms)) {
      if (!is.null(transforms[[i]]$name) && is.character(transforms[[i]]$name)) {
        tname <- transforms[[i]]$name;
      } else {
        tname <- NULL;
      }

      f <- transforms[[i]]$transform;
      if (rlang::is_primitive(f)) { # the function is_primitive is rather new in rlang; it didn't exist in V0.2.2, but it exists in V0.4.6 - I don't know when it appeared.
        pname <- rlang::prim_name(f);
      } else {
        pname <- NULL;

        # see if there is just one function call in the body of this non-primitive function, and if so, return that.
        try({
          x <- rlang::fn_body(f);
          if (length(x) == 2) {
            pname <- rlang::expr_name(x[[2]][[1]]);
          }
        })
      }

      fname[i] <- switch(
        is.null(tname) + is.null(pname) + 1,
        switch(
          prefer_primitive,
          if_shorter = if (nchar(pname) < nchar(tname)) { pname; } else { tname; },
          never = tname,
          always = pname
        ),
        c(tname, pname),
        'f.'
      )
    }
  }

  names(fname) <- names(transforms);
  fname;
}

#' Add simulated values to a dataset conditionally, based on a \code{simbase_*} object
#'
#' @details
#'
#' Given a \code{simbase_*} object, this function adds simulated values to a
#' dataset, conditional on the values of some of the variables already
#' contained in the dataset.
#'
#' @param data The dataset where simulated values are added to.
#'  The dataset has to contain at least one variable which is also included in
#'  the \code{simbase_*} object.
#' @param simbase Basic data object for the simulation, as calculated e.g.
#'   by [simbase_covar()] or [simbase_list()].
#' @param force_positive If \code{TRUE}, the resulting values are forced
#'   to be \eqn{\ge 0}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The modified dataset \code{data} with simulated values.
#'
#' @examples
#' # add simulated tension data based on a simbase stored in WoodSimulatR
#' dataset <- data.frame(E_dyn = rnorm(n = 100, mean = 12500, sd = 2200));
#' dataset_t <- simulate_conditionally(dataset, ws_t)
#'
#' # add simulated bending data
#' dataset_be <- simulate_conditionally(dataset, ws_be)
#'
#' @export
simulate_conditionally <- function(data, simbase, force_positive=TRUE, ...) {
  UseMethod('simulate_conditionally', simbase);
}

#' @importFrom dplyr bind_cols
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom tibble as_tibble
#' @importFrom rlang syms
#' @importFrom rlang sym
#' @importFrom rlang :=
#' @importFrom stats rnorm
#'
#' @export
simulate_conditionally.simbase_covar <- function(data, simbase, force_positive=TRUE, ...) {
  # to know what is already available in the dataset and what should be
  # simulated, we look at the variable names in data
  available_vars <- intersect(names(data), simbase$variables);
  stopifnot(length(available_vars) > 0);

  to_simulate <- setdiff(simbase$variables, names(data));
  stopifnot(length(to_simulate) > 0);

  # to make everything work as expected, we force the variables in simbase
  # into the following order: first available_vars, then to_simulate
  i <- c(available_vars, to_simulate);
  simbase$variables <- i;
  simbase$covar <- simbase$covar[i, i];
  simbase$means <- simbase$means[i];


  # Collect the available_vars in a new dataset and apply any transforms
  # grouping variables cannot be removed; therefore, we remove any grouping first.
  data_pred <- dplyr::select(dplyr::ungroup(data), !!! rlang::syms(available_vars))
  for (i in intersect(names(simbase$transforms), available_vars)) {
    data_pred[, i] <- do.call(simbase$transforms[[i]]$transform, list(data_pred[, i]));
  }

  # partition means and covariance matrix into "available_vars" and "to_simulate"
  # for subsetting the covariance matrix, we have to add "drop=FALSE"; otherwise,
  # the names will be lost when we subset only one row and/or only one column.
  m_pred <- simbase$means[available_vars];
  m_gdp  <- simbase$means[to_simulate];

  cov_pp <- simbase$covar[available_vars, available_vars, drop = FALSE];
  cov_pg <- simbase$covar[available_vars, to_simulate,    drop = FALSE];
  cov_gp <- simbase$covar[to_simulate,    available_vars, drop = FALSE];
  cov_gg <- simbase$covar[to_simulate,    to_simulate,    drop = FALSE];

  # we also need the inverse of the predictor's covariance matrix
  # XXX: maybe it would be better to use the correlation matrix here to get
  #      more stable inverses?
  cov_pp_inv <- solve(cov_pp);

  # get the conditional covariance matrix
  cov_gg_cond <- cov_gg - cov_gp %*% cov_pp_inv %*% cov_pg;

  # get the conditional means -- separately for each row in data!
  # The formula expects an n_variables x 1 vector -- therefore, we generate an
  # n_variables x n_data_points matrix.
  # To transform m_pred and m_gdp to n_data_points wide matrices, we multiply
  # them with a 1 x n_data_points matrix.
  # XXX ATTENTION: for large datasets "data", this might lead to memory problems!
  mat_pred <- t(as.matrix(data_pred));
  mat_ones <- matrix(1, nrow = 1, ncol = nrow(data));
  m_pred <- matrix(m_pred, nrow = length(m_pred)) %*% mat_ones;
  m_gdp  <- matrix(m_gdp,  nrow = length(m_gdp))  %*% mat_ones;
  m_gdp_cond <- m_gdp + cov_gp %*% cov_pp_inv %*% (mat_pred - m_pred);

  # Simulate correlated variables with covariance matrix cov_gg_cond
  mat_H <- chol(cov_gg_cond);

  # Generate independent random normal variables.
  mat_X <- matrix(stats::rnorm(length(to_simulate)*nrow(data)), ncol=length(to_simulate));

  # multiply with mat_H to get correlated random variables with covariance
  # matrix cov_gg_cond
  mat_X <- mat_X %*% mat_H;

  # add conditional means to get the final values (apart from transformations)
  mat_X <- mat_X + t(m_gdp_cond);

  # Put the new variables in the dataset
  data <- dplyr::bind_cols(data, tibble::as_tibble(mat_X));

  # execute the back-transforms (but only for variables in to_simulate!)
  for (i in intersect(to_simulate, names(simbase$transforms))) {
    data[, i] <- do.call(simbase$transforms[[i]]$inverse, list(data[, i]));
  }

  # force positive results (but only for variables in to_simulate!)
  if (force_positive == TRUE) {
    for (x in to_simulate) {
      data <- dplyr::mutate(data, !!rlang::sym(x) := ifelse(!!rlang::sym(x) < 0, 0, !!rlang::sym(x)))
    }
  }

  data
}

#' Add simulated values to a dataset conditionally, based on a \code{simbase_list} object
#'
#' @details Simulating values based on a \code{\link{simbase_list}} object
#' has some special aspects compared to that of other \code{simbase_*} objects,
#' (see [simulate_conditionally()]).
#'
#' In particular, a \code{\link{simbase_list}} object stores \code{simbase}s
#' for specific value combinations within the grouping variables.
#'
#' These grouping variables must also be present in \code{data}.
#'
#' If there is a value combination in these grouping variables for which no
#' dedicated \code{simbase} object exists, this will lead to \code{NA} values
#' in the columns to be simulated and either to an error
#' (if \code{error_when_groups_missing = TRUE}) or to a warning.
#'
#' Due to the internal call to [tidyr::nest()] and subsequent call to
#' [tidyr::unnest()], the returned dataset will be ordered according to
#' the grouping variables in the simbase, with any grouping variable
#' combinations missing in the simbase coming last.
#'
#' @param data The dataset where simulated values are added to.
#' @param simbase Basic data object for the simulation, as calculated by
#'  \code{\link{simbase_list}}.
#' @param force_positive If \code{TRUE}, the resulting values are forced
#'   to be \eqn{\ge 0}.
#' @param ... further arguments passed to or from other methods.
#' @param error_when_groups_missing Whether to raise an error if for a certain
#'  value combination in the grouping variables no dedicated \code{simbase}
#'  exists (see details).
#'
#' @return The modified dataset \code{data} with simulated values.
#'
#' @examples
#' # create a simbase_list object for the values of subsets = c('AT', 'DE')
#' dataset_0 <- simulate_dataset(subsets = c('AT', 'DE'));
#' simbase <- simbase_covar(dplyr::group_by(dataset_0, country), c('f', 'E', 'E_dyn'));
#'
#' # simulate on another dataset
#' dataset <- data.frame(E_dyn = rnorm(n = 100, mean = 12500, sd = 2200), country = 'AT');
#' dataset_1 <- simulate_conditionally(dataset, simbase);
#' head(dataset_1);
#'
#' # warning if for some value of country we don't have an entry in the simbase
#' dataset$country <- 'CH';
#' dataset_2 <- simulate_conditionally(dataset, simbase, error_when_groups_missing = FALSE);
#' head(dataset_2);
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr group_nest
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map_int
#' @importFrom purrr map2
#' @importFrom rlang .data
#' @importFrom rlang sym
#' @importFrom rlang syms
#' @importFrom rlang :=
#' @importFrom tidyr unnest
#' @export
simulate_conditionally.simbase_list <- function(data, simbase, force_positive=TRUE, ..., error_when_groups_missing = TRUE) {
  .simbase <- NULL; # due to NSE notes in R CMD check

  grpv <- setdiff(names(simbase), '.simbase');
  if (!all(grpv %in% names(data))) {
    stop('the following grouping variables from simbase are missing in data: ',
         paste(setdiff(grpv, names(data)), collapse = ', '));
  }

  data <- dplyr::group_by(data, !!! rlang::syms(grpv));

  dsname <- make.unique(c(grpv, '.dataset'))[length(grpv) + 1];

  grpd <- dplyr::group_nest(data, .key = dsname);
  grpd <- dplyr::left_join(grpd, simbase, by = grpv);
  i <- purrr::map_int(grpd$.simbase, length);
  if (any(i == 0)) {
    if (error_when_groups_missing) {
      stop(sum(i==0), ' combinations of values of ', paste(grpv, collapse = ', '),
           ' in data are not found in simbase.');
    } else {
      warning(sum(i==0), ' combinations of values of ', paste(grpv, collapse = ', '),
              ' in data are not found in simbase. This will lead to NA values!');

      # add the NA values where no simbase entry is available
      grpd_0 <- tidyr::unnest(dplyr::select(grpd[i == 0, ], -.simbase), cols = dsname);
      n <- setdiff(variable.names(simbase), names(grpd_0));
      if (length(n) > 0) {
        n <- structure(rep(list(rlang::expr(NA)), length(n)), names = n);
        grpd_0 <- dplyr::mutate(grpd_0, !!! n);
      }

      # the simulation will be done on the rest only
      grpd <- grpd[i > 0, ];
    }
  } else {
    grpd_0 <- tibble::tibble();
  }

  if (any(i > 0)) {
    dssym <- rlang::sym(dsname);
    grpd <- dplyr::mutate(
      grpd,
      !! dssym := purrr::map2(
        .x = !! dssym,
        .y = .simbase,
        .f = simulate_conditionally,
        force_positive = force_positive,
        ...
      )
    );
    grpd <- tidyr::unnest(dplyr::select(grpd, -.simbase), cols = tidyselect::all_of(dsname));
    grpd <- dplyr::bind_rows(grpd, grpd_0);
  } else {
    grpd <- grpd_0;
  }

  grpd;
}

#' @export
#' @importFrom stats variable.names
variable.names.simbase_covar <- function(object, ...) {
  object$variables;
}

#' @export
#' @importFrom stats variable.names
variable.names.simbase_list <- function(object, ...) {
  # we return the union of all entries
  n <- purrr::map(object$.simbase, variable.names);
  n <- purrr::flatten_chr(n);
  unique(n);
}
