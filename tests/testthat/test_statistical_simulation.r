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
