## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5
)

## ----setup--------------------------------------------------------------------
library(WoodSimulatR)
library(magrittr)
library(ggplot2)
pander::panderOptions('knitr.auto.asis', FALSE);

## -----------------------------------------------------------------------------
summ_fun <- function(ds, grp = c('country', 'subsample', 'loadtype')) {
  grp <- intersect(grp, names(ds));
  v <- setdiff(names(ds), grp);
  
  r <- cor(ds[v]);

  ds <- tibble::add_column(ds, n = 1);
  v <- c('n', v);
  ds <- tidyr::gather(ds, 'property', 'value', !!! rlang::syms(v));
  ds <- dplyr::mutate(
    ds,
    property = factor(
      property,
      levels=v,
      labels=ifelse(v=='n', v, paste0(v, '_mean')),
      ordered = TRUE
    )
  );
  
  grp <- c(grp, 'property');
  ds <- dplyr::group_by(ds, !!! rlang::syms(grp));
  
  summ <- dplyr::summarise(
    ds,
    res = if (property[1] == 'n') sprintf('%.0f', sum(value)) else
      sprintf(
      if(property[1] %in% c('f_mean', 'ip_f_mean')) '%.1f (%.0f)' else '%.0f (%.0f)',
      mean(value), 100*sd(value)/mean(value)),
    .groups = 'drop_last'
  );
  pander::pander(
    tidyr::spread(summ, property, res),
    split.tables = Inf
  );
  
  pander::pander(r)
  
  invisible(summ);
}

compare_with_def <- function(ds, ssd, target = c('mean', 'cov')) {
  target <- match.arg(target);
  
  ds <- dplyr::group_by(ds, country);
  summ <- dplyr::summarise(
    ds,
    f_mean.ach = mean(f),
    f_cov.ach = sd(f) / f_mean.ach,
    E_mean.ach = mean(E),
    E_cov.ach = sd(E) / E_mean.ach,
    rho_mean.ach = mean(rho),
    rho_cov.ach = sd(rho) / rho_mean.ach,
    .groups = 'drop_last'
  );
  
  stopifnot(!anyDuplicated(ssd$country));
  summ <- dplyr::left_join(
    summ,
    dplyr::select(
      dplyr::mutate(ssd, f_cov = f_sd / f_mean, E_cov = E_sd / E_mean, rho_cov = rho_sd / rho_mean), 
      country, f_mean, f_cov, E_mean, E_cov, rho_mean, rho_cov
    ),
    by = 'country'
  );
  
  summ <- tidyr::pivot_longer(
    summ,
    -country,
    names_to = c('gdpname', '.value'),
    names_sep = '_'
  );
  summ <- dplyr::mutate(
    summ,
    gdpname = factor(gdpname, levels = c('f', 'E', 'rho'), ordered = TRUE)
  );

  if (target == 'mean') {
    ggplot(data = summ, aes(mean.ach, mean)) +
      geom_abline(slope = 1, intercept = 0) +
      geom_text(aes(label = country)) +
      geom_point(alpha = 0.5) +
      facet_wrap(vars(gdpname), scales = 'free') +
      theme(axis.text.x = element_text(angle = 90));
  } else {
    ggplot(data = summ, aes(cov.ach, cov)) +
      geom_abline(slope = 1, intercept = 0) +
      geom_text(aes(label = country)) +
      geom_point(alpha = 0.5) +
      facet_wrap(vars(gdpname), scales = 'free') +
      theme(axis.text.x = element_text(angle = 90));
  }
}

## ----results='asis'-----------------------------------------------------------
dataset_0 <- simulate_dataset(random_seed = 2345);

summ_fun(dataset_0);

## ----results='asis'-----------------------------------------------------------
get_subsample_definitions(loadtype = 't') %>% 
  dplyr::select(-species, -loadtype) %>%
  dplyr::arrange(country) %>%
  pander::pander(split.table = Inf);

## ----results='asis'-----------------------------------------------------------
get_subsample_definitions(loadtype = 'be') %>% 
  dplyr::select(-species, -loadtype) %>%
  dplyr::arrange(country) %>%
  pander::pander(split.table = Inf);

## ----results='asis'-----------------------------------------------------------
ssd_c <- get_subsample_definitions(
  country = c('at', 'de', 'fi', 'pl', 'se', 'si', 'sk'),
  loadtype = 't'
);

dataset_c <- simulate_dataset(
  random_seed = 12345,
  n = 5000,
  subsets = ssd_c
);

summ_fun(dataset_c);

## -----------------------------------------------------------------------------
compare_with_def(dataset_c, ssd_c, 'm')

## -----------------------------------------------------------------------------
compare_with_def(dataset_c, ssd_c, 'cov')

## ----results='asis'-----------------------------------------------------------
ssd_cn <- get_subsample_definitions(
  country = c(at = 1, de = 3, fi = 1.5, pl = 2, se = 3, si = 1, sk = 1),
  loadtype = 't'
);

dataset_cn <- simulate_dataset(
  random_seed = 12345,
  n = 5000,
  subsets = ssd_cn
);

summ_fun(dataset_cn);

## -----------------------------------------------------------------------------
ssd_custom <- tibble::tribble(
  ~width, ~thickness, ~f_mean, ~f_sd,
      80,     40,      27.5,    9.0,
     140,     40,      29.4,    9.7,
     160,     60,      31.6,    9.3,
     200,     50,      30.2,   11.4, 
     240,     95,      25.5,    4.8,
     250,     40,      25.3,   11.2
);

dataset_custom <- simulate_dataset(
  random_seed = 12345,
  n = 5000,
  subsets = ssd_custom
);

summ_fun(dataset_custom, grp = c('width', 'thickness', 'loadtype'));

## -----------------------------------------------------------------------------
plot_sim_gdp <- function(ds, simb, simulated_vars, ...) {
  extra_aes <- rlang::enexprs(...);
  ds <- dplyr::rename(ds, f_ref = f, E_ref = E, rho_ref = rho);
  if (!any(simulated_vars %in% names(ds))) ds <- simulate_conditionally(data = ds, simbase = simb);
  ds <- tidyr::pivot_longer(
    data = ds,
    cols = tidyselect::any_of(c('f_ref', 'E_ref', 'rho_ref', simulated_vars)),
    names_to = c('name', '.value'),
    names_sep = '_'
  );
  ds <- dplyr::mutate(
    ds,
    name = factor(name, levels = c('f', 'E', 'rho'), ordered = TRUE)
  );
  simname <- names(ds);
  simname <- simname[dplyr::cumany(simname == 'name')];
  simname <- setdiff(simname, c('name', 'ref'));
  stopifnot(length(simname) == 1);
  ggplot(data = ds, mapping = aes(.data[[simname]], ref, !!!extra_aes)) +
    geom_point(alpha = .2, shape = 20) +
    geom_abline(slope = 1, intercept = 0, alpha = .5, linetype = 'twodash') +
    facet_wrap(vars(name), scales = 'free') +
    theme(axis.text.x = element_text(angle = 90));
} # undebug(plot_sim_gdp)

## -----------------------------------------------------------------------------
sb_untransf <- dataset_0 %>%
  dplyr::rename(f_siml = f, E_siml = E, rho_siml = rho) %>%
  simbase_covar(
    variables = c('f_siml', 'E_siml', 'rho_siml', 'ip_f', 'E_dyn', 'ip_rho')
  );

sb_untransf;

## ----results='asis'-----------------------------------------------------------
dataset_c_sim <- simulate_conditionally(dataset_c, sb_untransf);
names(dataset_c_sim) %>% pander::pander();

## -----------------------------------------------------------------------------
plot_sim_gdp(dataset_c_sim, sb_untransf, c('f_siml', 'E_siml', 'rho_siml'));

## -----------------------------------------------------------------------------
sb_transf <- dataset_0 %>%
  dplyr::rename(f_simt = f, E_simt = E, rho_simt = rho) %>%
  simbase_covar(
    variables = c('f_simt', 'E_simt', 'rho_simt', 'ip_f', 'E_dyn', 'ip_rho'),
    transforms = list(f_simt = scales::log_trans())
  );
dataset_c_sim <- simulate_conditionally(dataset_c_sim, sb_transf);
plot_sim_gdp(dataset_c_sim, sb_transf, c('f_simt', 'E_simt', 'rho_simt'));

## -----------------------------------------------------------------------------
sb_group <- dataset_0 %>%
  dplyr::group_by(country) %>%
  dplyr::rename(f_simg = f, E_simg = E, rho_simg = rho) %>%
  simbase_covar(
    variables = c('f_simg', 'E_simg', 'rho_simg', 'ip_f', 'E_dyn', 'ip_rho'),
    transforms = list(f_simg = scales::log_trans())
  );

sb_group

## -----------------------------------------------------------------------------
dataset_0_sim <- simulate_conditionally(dataset_0, sb_group);
plot_sim_gdp(dataset_0_sim, sb_group, c('f_simg', 'E_simg', 'rho_simg'), colour=country);

## -----------------------------------------------------------------------------
sb_group_c <- dataset_c %>%
  dplyr::group_by(country) %>%
  simbase_covar(
    variables = c('f', 'E', 'rho', 'ip_f', 'E_dyn', 'ip_rho'),
    transforms = list(f = scales::log_trans())
  );

sb_group_c

## ----results='asis'-----------------------------------------------------------
dataset_cn2 <- simulate_dataset(
  random_seed = 12345,
  n = 5000,
  subsets = ssd_cn,
  simbase = sb_group_c
);

summ_fun(dataset_cn2);

