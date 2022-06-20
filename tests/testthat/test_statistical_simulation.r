test_that('transform names are returned correctly', {
  expect_identical(
    get_transform_names(list(x = list(name = 'my name', transform = log))),
    c(x = 'log')
  );

  expect_identical(
    get_transform_names(list(x = list(name = 'lg', transform = log))),
    c(x = 'lg')
  );

  expect_identical(
    get_transform_names(list(x = list(name = 'my name', transform = function(x) log(x)))),
    c(x = 'log')
  );

});

test_that('force_positive works with different simbase classes in simulate_conditionally()', {
  force_positive_works <- function(simbase_class, force_positive) {
    data <- tibble::tibble(rho = c(-400, -350, -300), grp = 'A');
    simbase <- switch(
      simbase_class,
      'simbase_covar' = ws_t,
      'simbase_list' = {
        i <- tibble::tibble(grp = 'A', .simbase = list(ws_t));
        class(i) <- c('simbase_list', class(i));
        i;
      }
    );

    data2 <- simulate_conditionally(data, simbase, force_positive = force_positive);

    if (force_positive) {
      all(data2$f == 0);
    } else {
      all(data2$f < 0);
    }
  }

  expect_true(force_positive_works(simbase_class = 'simbase_covar', force_positive = TRUE));
  expect_true(force_positive_works(simbase_class = 'simbase_covar', force_positive = FALSE));
  expect_true(force_positive_works(simbase_class = 'simbase_list', force_positive = TRUE));
  expect_true(force_positive_works(simbase_class = 'simbase_list', force_positive = FALSE));
});

test_that('simulate_conditionally() works even with a single row to predict', {
  data <- tibble::tibble(rho = 400);
  expect_equal(sum(is.na(simulate_conditionally(data, simbase = ws_t))), 0);

  data <- tibble::tibble(grp = c('A', 'B'), rho = c(400, 500));
  sb <- structure(
    tibble::tibble(grp = c('A', 'B'), .simbase = list(ws_t_logf, ws_be_logf)),
    class = c('simbase_list', class(data))
  );
  expect_equal(sum(is.na(simulate_conditionally(data, simbase = sb))), 0);

  data <- tibble::tibble(f = 25);
  expect_equal(sum(is.na(simulate_conditionally(data, simbase = ws_t_logf))), 0);

});

