# WoodSimulatR 0.6.2

* Update the links in the documentation, especially those which link to
  functions in other packages, so that `roxygen2` automatically creates the
  correct links.
  This is relevant for functions which are documented under topics which differ
  from the function name.
  At the moment, however, this is mostly a cosmetic update.

# WoodSimulatR 0.6.1

* `simbase_covar()` allows the use of transformations, for which it relies on
  the package `scales`.
  It verifies that the transformation are of class `trans`.
  In version 1.3.0 of `scales`, the class name of transformation was renamed
  from `trans` to `transform`.
    + As an intermediate solution, both class names `trans` and `transform` are
      now allowed.
    + The `simbase` objects stored in `WoodSimulatR` were updated to the new
      class name `transform`.
* Removed the `.data` pronoun, because it is deprecated in `tidyselect`
  expressions since `tidyselect` version 1.2.0.
  As an alternative way to remove the NOTE about "undefined global variables"
  which are actually due to non-standard evaluation, the variables in question
  are set to `NULL` at the beginning of each involved function, as recommended
  by the vignette "Importing data.table" from the `data.table` package.
* Corrected a typo in the help for `simbase_covar()`

# WoodSimulatR 0.6.0

* Major overhaul of `simulate_dataset()`:
    + Corrected bug that created errors when only one
      variable was defined by `*_mean` and `*_sd` in the argument `subsets`.
    + Made warning and error messages more explicit and clear.
    + Added examples.
    + Added a parameter `RNGversion` and changed the default
      of `RNGversion` from `"3.5.0"` to `"3.6.0"`.
      This followed an improvement in random number generation that was
      introduced in R version 3.6.0 (see the R help on `RNGversion()`).
      The new parameter `RNGversion` was added to enable perfect reproducibility
      of simulated data sets generated with earlier versions of `WoodSimulatR`.
    + Added the possibility to use different identifier columns in the `subsets`
      parameter.
    + Updated the documentation accordingly and corrected some typos 
      -- in particular, the documentation mentioned a
      "WoodGradeR" package, but it should be "WoodSimulatR".
* Removed bug which meant that `force_positive=FALSE` didn't work with
  objects of class `simbase_list`.
* Added information on the origin and species of the underlying sawn timber
  in the help on `simbase`.
* Added the unit "N/mm^2" of the variable `ip_f` in the help on `simbase`.
* Added `Date` entry in the `DESCRIPTION`.
* Added link to Holzforschung Austria in README.md, which got lost due to a
  mistake in README.Rmd.
* Slight update to the vignette "woodsimulatr_basics", especially to show the
  new possibility to use different identifier columns in the call to
  `simulate_dataset()`.

# WoodSimulatR 0.5.0

* First public release.
