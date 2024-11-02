# Test GHA v1.3.6
# Test case adapted from https://github.com/Rdatatable/data.table/issues/6105#issue-2268691745 which is where the issue was reported.
# https://github.com/Rdatatable/data.table/pull/6107 fixed performance across 3 ways to specify a column as Date, and we test each individually.
extra.args.6107 <- c(
  "colClasses=list(Date='date')",
  "colClasses='Date'",
  "select=list(Date='date')")
extra.test.list <- list()
for (extra.arg in extra.args.6107){
  this.test <- atime::atime_test(
    setup = {
      set.seed(1)
      DT = data.table(date=.Date(sample(20000, N, replace=TRUE)))
      tmp_csv = tempfile()
      fwrite(DT, tmp_csv)
    },
    Slow = "e9087ce9860bac77c51467b19e92cf4b72ca78c7", # Parent of the merge commit (https://github.com/Rdatatable/data.table/commit/a77e8c22e44e904835d7b34b047df2eff069d1f2) of the PR (https://github.com/Rdatatable/data.table/pull/6107) that fixes the issue
    Fast = "a77e8c22e44e904835d7b34b047df2eff069d1f2") # Merge commit of the PR (https://github.com/Rdatatable/data.table/pull/6107) that fixes the issue
  this.test$expr = str2lang(sprintf("data.table::fread(tmp_csv, %s)", extra.arg))
  extra.test.list[[sprintf("fread(%s) improved in #6107", extra.arg)]] <- this.test
}

# Test case adapted from https://github.com/Rdatatable/data.table/pull/4386#issue-602528139 which is where the performance was improved.
for(retGrp_chr in c("T","F"))extra.test.list[[sprintf(
  "forderv(retGrp=%s) improved in #4386", retGrp_chr
)]] <- list(
  setup = quote({
    dt <- data.table(group = rep(1:2, l=N))
  }),
  expr = substitute({
    old.opt <- options(datatable.forder.auto.index = TRUE) # required for test, un-documented, comments in forder.c say it is for debugging only.
    data.table:::forderv(dt, "group", retGrp = RETGRP)
    options(old.opt) # so the option does not affect other tests.
  }, list(RETGRP=eval(str2lang(retGrp_chr)))),
  ## From ?bench::mark, "Each expression will always run at least twice,
  ## once to measure the memory allocation and store results
  ## and one or more times to measure timing."
  ## So for atime(times=10) that means 11 times total.
  ## First time for memory allocation measurement,
  ## (also sets the index of dt in this example),
  ## then 10 more times for time measurement.
  ## Timings should be constant if the cached index is used (Fast),
  ## and (log-)linear if the index is re-computed (Slow).
  Slow = "b1b1832b0d2d4032b46477d9fe6efb15006664f4", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/b0efcf59442a7d086c6df17fa6a45c81b082322e) in the PR (https://github.com/Rdatatable/data.table/pull/4386/commits) where the performance was improved.
  Fast = "ffe431fbc1fe2d52ed9499f78e7e16eae4d71a93" # Last commit of the PR (https://github.com/Rdatatable/data.table/pull/4386/commits) where the performance was improved.
)

# A list of performance tests.
#
# See documentation in https://github.com/Rdatatable/data.table/wiki/Performance-testing for best practices.
#
# Each entry in this list corresponds to a performance test and contains a sublist with three mandatory arguments:
# - N: A numeric sequence of data sizes to vary.
# - setup: An expression evaluated for every data size before measuring time/memory.
# - expr: An expression that will be evaluated for benchmarking performance across different git commit versions.
#         This must call a function from data.table using a syntax with double or triple colon prefix.
#         The package name before the colons will be replaced by a new package name that uses the commit SHA hash.
#         (For instance, data.table:::[.data.table will become data.table.some_40_digit_SHA1_hash:::[.data.table)
#
# Optional parameters that may be useful to configure tests:
# - times: Number of times each expression is evaluated (default is 10).
# - seconds.limit: The maximum median timing (in seconds) of an expression. No timings for larger N are computed past that threshold. Default of 0.01 seconds should be sufficient for most tests.
# - Historical references (short version name = commit SHA1).
#   For historical regressions, use version names 'Before', 'Regression', and 'Fixed'
#   When there was no regression, use 'Slow' and 'Fast' 
# @note Please check https://github.com/tdhock/atime/blob/main/vignettes/data.table.Rmd for more information.
# nolint start: undesirable_operator_linter. ':::' needed+appropriate here.
test.list <- atime::atime_test_list(
  # Common N and pkg.edit.fun are defined here, and inherited in all test cases below which do not re-define them.
  N = as.integer(10^seq(1, 7, by=0.25)),
  # A function to customize R package metadata and source files to facilitate version-specific installation and testing.
  #
  # This is specifically tailored for handling data.table which requires specific changes in non-standard files (such as the object file name in Makevars and version checking code in onLoad.R)
  # to support testing across different versions (base and HEAD for PRs, commits associated with historical regressions, etc.) of the package.
  # It appends a SHA1 hash to the package name (PKG.SHA), ensuring each version can be installed and tested separately.
  #
  # @param old.Package Current name of the package.
  # @param new.Package New name of the package, including a SHA hash.
  # @param sha SHA1 hash used for differentiating versions.
  # @param new.pkg.path Path to the package files.
  #
  # @details
  # The function modifies:
  # - DESCRIPTION, updating the package name.
  # - Makevars, customizing the shared object file name and adjusting the build settings.
  # - R/onLoad.R, adapting custom version checking for package loading operations.
  # - NAMESPACE, changing namespace settings for dynamic linking.
  #
  # @examples
  # pkg.edit.fun("data.table", "data.table.some_SHA1_hash", "some_SHA1_hash", "/path/to/data.table")
  #
  # @return None (performs in-place file modifications)
  # @note This setup is typically unnecessary for most packages but essential for data.table due to its unique configuration.
  pkg.edit.fun = function(old.Package, new.Package, sha, new.pkg.path) {
    pkg_find_replace <- function(glob, FIND, REPLACE) {
      atime::glob_find_replace(file.path(new.pkg.path, glob), FIND, REPLACE)
    }
    Package_regex <- gsub(".", "_?", old.Package, fixed = TRUE)
    Package_ <- gsub(".", "_", old.Package, fixed = TRUE)
    new.Package_ <- paste0(Package_, "_", sha)
    pkg_find_replace(
      "DESCRIPTION",
      paste0("Package:\\s+", old.Package),
      paste("Package:", new.Package))
    pkg_find_replace(
      file.path("src", "Makevars.*in"),
      Package_regex,
      new.Package_)
    pkg_find_replace(
      file.path("R", "onLoad.R"),
      Package_regex,
      new.Package_)
    pkg_find_replace(
      file.path("R", "onLoad.R"),
      sprintf('packageVersion\\("%s"\\)', old.Package),
      sprintf('packageVersion\\("%s"\\)', new.Package))
    pkg_find_replace(
      file.path("src", "init.c"),
      paste0("R_init_", Package_regex),
      paste0("R_init_", gsub("[.]", "_", new.Package_)))
    pkg_find_replace(
      "NAMESPACE",
      sprintf('useDynLib\\("?%s"?', Package_regex),
      paste0('useDynLib(', new.Package_))
  },

  tests=extra.test.list)
# nolint end: undesirable_operator_linter.
