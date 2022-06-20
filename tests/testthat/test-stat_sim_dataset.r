test_that('subset specification by data frame works correctly in simulate_dataset()', {
  sdef <- data.frame(country = 'a', subsample = 'a', f_mean = 20, f_sd = 5);
  expect_equal(mean(simulate_dataset(subsets = sdef, simbase = ws_t_logf)$f), sdef$f_mean, tolerance = 0.1);
  sdef <- data.frame(country = 'a', subsample = 'a', f_mean = 20, f_sd = 5, E_mean = 10000, E_sd = 2000);
  expect_equal(mean(simulate_dataset(subsets = sdef, simbase = ws_t_logf)$f), sdef$f_mean, tolerance = 0.1);

  # the following is only approximately true and could therefore lead to problems from time to time:
  # although, the high tolerance makes this very unlikely...
  expect_equal(mean(simulate_dataset(subsets = sdef, simbase = ws_t_logf)$E), sdef$E_mean, tolerance = 0.1);

  # alternative identifier columns
  sdef <- data.frame(Land = 'a', Region = 'a', f_mean = 20, f_sd = 5);
  expect_equal(mean(simulate_dataset(subsets = sdef, simbase = ws_t_logf)$f), sdef$f_mean, tolerance = 0.1);
});

test_that('share specification works correctly in simulate_dataset()', {
  # TODO: add also different f_mean values and check those as well
  sdef <- data.frame(id = factor(c('a', 'b')), share = c(1000, 4000), f_mean = 20, f_sd = 5);
  expect_equal(
    summary(simulate_dataset(subsets = sdef, simbase = ws_t_logf)$id),
    structure(sdef$share, names = as.character(sdef$id))
  );

  # we can also just pass in the number of desired subsamples
  # XXX doesn't perfectly fit in here
  expect_equal(
    length(unique(simulate_dataset(subsets = 3, simbase = ws_t_logf, n = 10)$subsample)),
    3
  );
  # we can also just pass in the desired share values
  expect_equal(
    summary(factor(simulate_dataset(subsets = c(3, 7), simbase = ws_t_logf, n = 10)$subsample)),
    c(C1 = 3, C2 = 7)
  );
});

test_that('simulation works even if only a single row is created for some subsamples in simulate_dataset()', {
  # if no transform is involved, this works out of the box
  expect_equal(
    sum(is.na(simulate_dataset(subsets = c(1, 1, 8), simbase = ws_be, n = 10))),
    0
  );

  # but with transforms involved, we had a bug in version 0.5.0.
  # we had to divide by the actual standard deviation, which is NA for a single
  # observation...
  expect_equal(
    sum(is.na(simulate_dataset(subsets = c(1, 1, 8), simbase = ws_t_logf, n = 10))),
    0
  );
});
