# A list of performance tests.
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

  # Performance regression discussed in: https://github.com/Rdatatable/data.table/issues/4311
  # Fixed in: https://github.com/Rdatatable/data.table/pull/4440
  "shallow regression fixed in #4440" = atime::atime_test(
    N = 10^seq(3,8),
    setup = {
      set.seed(1L)
      dt <- data.table(a = sample.int(N))
      setindexv(dt, "a")
    },
    expr = data.table:::shallow(dt),
    # Before = "", This needs to be updated later as there are two issues here: A) The source of regression (or the particular commit that led to it) is not clear; B) Older versions of data.table are having problems when being installed in this manner (This includes commits from before March 20 2020, when the issue that discovered or first mentioned the regression was created)
    Regression = "b1b1832b0d2d4032b46477d9fe6efb15006664f4", # Parent of the first commit (https://github.com/Rdatatable/data.table/commit/0f0e7127b880df8459b0ed064dc841acd22f5b73) in the PR (https://github.com/Rdatatable/data.table/pull/4440/commits) that fixes the regression
    Fixed = "9d3b9202fddb980345025a4f6ac451ed26a423be"), # Merge commit in the PR that fixed the regression (https://github.com/Rdatatable/data.table/pull/4440)

  # Test based on: https://github.com/Rdatatable/data.table/issues/5424
  # Performance regression introduced from a commit in: https://github.com/Rdatatable/data.table/pull/4491
  # Fixed in: https://github.com/Rdatatable/data.table/pull/5463
  "memrecycle regression fixed in #5463" = atime::atime_test(
    N = 10^seq(3, 8),
    setup = {
      n <- N/100
      set.seed(2L)
      dt <- data.table(
        g = sample(seq_len(n), N, TRUE),
        x = runif(N),
        key = "g")
      dt_mod <- copy(dt)
    },
    expr = data.table:::`[.data.table`(dt_mod, , N := .N, by = g),
    Before = "be2f72e6f5c90622fe72e1c315ca05769a9dc854", # Parent of the regression causing commit (https://github.com/Rdatatable/data.table/commit/e793f53466d99f86e70fc2611b708ae8c601a451) in the PR that introduced the issue (https://github.com/Rdatatable/data.table/pull/4491/commits)
    Regression = "e793f53466d99f86e70fc2611b708ae8c601a451", # Commit responsible for regression in the PR that introduced the issue (https://github.com/Rdatatable/data.table/pull/4491/commits)
    Fixed = "58409197426ced4714af842650b0cc3b9e2cb842"), # Last commit in the PR that fixed the regression (https://github.com/Rdatatable/data.table/pull/5463/commits)

  # Issue reported in: https://github.com/Rdatatable/data.table/issues/5426
  # To be fixed in: https://github.com/Rdatatable/data.table/pull/5427
  "setDT improved in #5427" = atime::atime_test(
    N = 10^seq(1, 7),
    setup = {
      L <- replicate(N, 1, simplify = FALSE)
      setDT(L)
    },
    expr = {
      data.table:::setattr(L, "class", NULL)
      data.table:::setDT(L)
    },
    Slow = "c4a2085e35689a108d67dacb2f8261e4964d7e12", # Parent of the first commit in the PR that fixes the issue (https://github.com/Rdatatable/data.table/commit/7cc4da4c1c8e568f655ab5167922dcdb75953801)
    Fast = "1872f473b20fdcddc5c1b35d79fe9229cd9a1d15"), # Last commit in the PR that fixes the issue (https://github.com/Rdatatable/data.table/pull/5427/commits)

  # Remaining test cases (PRs to be sent) as per https://github.com/DorisAmoakohene/PerformanceTest_data.table/blob/master/README.md
  "[1, 2]" = atime::atime_test(
    N = 10^seq(1, 4),
    setup = { 
    set.seed(123L)
    dt <- data.table(
      id = seq_len(N),
      val = rnorm(N))
    dt
  },
 expr = data.table:::`[.data.table`(dt, , .(vs = (sum(val))), by = .(id)),
  Before = "be2f72e6f5c90622fe72e1c315ca05769a9dc854",
  Regression = "e793f53466d99f86e70fc2611b708ae8c601a451",
  Fixed = "58409197426ced4714af842650b0cc3b9e2cb842"),

  #"[3]" = atime::atime_test(  
  # N = 10^seq(1, 20),
  # setup = { 
  #  allIterations <- data.frame(v1 = runif(N), v2 = runif(N))
  #  DoSomething <- function(row) {
  #    someCalculation <- row[["v1"]] + 1
  #  }
  #    allIteration_dt <- as.data.table(allIterations)
  #    setDTthreads(1)
  # },
  # expr = {
  #  for(r in 1:nrow(allIterations)) {
  #    DoSomething(data.table:::`[.data.table`(allIterations, r, ))
  #  }
  # },
  # Slow = "d47a83fb2e25582e508f191f87a31ca81b736b57", # Parent of the first commit in the PR that fixes the issue (https://github.com/Rdatatable/data.table/pull/4488/commits)
  # Fast ="958e3dd3cba7c259220aa653bef4beb8ad74b239") # Last commit in the PR that fixes the issue (https://github.com/Rdatatable/data.table/pull/4488/commits)

   "[6]" = atime::atime_test( 
   N = 10^seq(1, 7),
   setup = { 
    DT = data.table(x = sample(N), y = sample(1e2,N,TRUE))
   },
   expr = data.table:::`[.data.table`(DT,, shift(x, 1, type = "lag"), y),
   Regression = "7f0ce147eef1dd5cd5ff05dffc3c72f472bcde51", # Parent of the first commits in the PR that fixes the issue6(https://github.com/Rdatatable/data.table/commit/58135017a985f3cc2c6f0d091c4effaec4442f56)
   Fixed = "a6abac319446ae7dde8bc4501fae40eeb5cc228c"), # Commit before the last commit in the PR that fixes the regression(https://github.com/Rdatatable/data.table/pull/5205/commits)

   "[7, 1]" = atime::atime_test(  
   N = 10^seq(1, 7),
   setup = { 
    n_nested = 40L
    dt = data.table(id = seq_len(N),
                    value = replicate(N, data.table(val1 = sample(n_nested)), simplify = FALSE))
   },
   expr = data.table:::`[.data.table`(dt, , value[[1L]], by = id),
   Slow = "db618444a4d16b91cccf35d80fa3f306739dd571", #parent of the first commit in the PR that fixes the issue(https://github.com/Rdatatable/data.table/pull/4655/commits)
   Fast = "ec041f78691f9f9495c0f25b78109cd3f5295f4f"), #This is the last commit in the PR that fixes the issue (https://github.com/Rdatatable/data.table/pull/4655/commits)

   "[7, 2]" = atime::atime_test( 
   N = 10^seq(1, 7),
   setup = { 
   dt <- data.table('id'= N,
                     'list_col'=sample(c('', '', 'a', 'a:b', 'a:b:c'), 20000, TRUE))
   feature <- 'list_col'
   },
   expr=data.table:::`[.data.table`(dt[, c("id", feature), with = FALSE][
    , feature_names := {
      x <- get(feature)
      stringr::str_split(x, ':')
    }][
      , .(
        feature_names = paste0(feature, "_", unlist(feature_names))
      )
      , by = "id"]
   , times = 10
   , unit = 'ms'),
   Slow = "db618444a4d16b91cccf35d80fa3f306739dd571", # Parent of the first commit in the PR that fixes the issue(https://github.com/Rdatatable/data.table/pull/4655/commits)
   Fast = "ec041f78691f9f9495c0f25b78109cd3f5295f4f") # This is the last commit in the PR that fixes the issue (https://github.com/Rdatatable/data.table/pull/4655/commits)
)
# nolint end: undesirable_operator_linter.
