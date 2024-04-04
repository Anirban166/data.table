pkg.edit.fun = quote(function(old.Package, new.Package, sha, new.pkg.path) {
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
    })

test.list <- list(
  # Performance regression fixed in: https://github.com/Rdatatable/data.table/pull/4440
  "Test regression fixed in #4440" = list(
    pkg.edit.fun = pkg.edit.fun,
    N = 10^seq(3,8),
    setup = quote({
      set.seed(1L)
      dt <- data.table(a = sample(N, N))
      setindex(dt, a)
    }),
    expr = quote(data.table:::shallow(dt)),
    #"Before"="", # Unknown source, and all the commit SHAs (each dating before March 20 '20, when the regression was noticed via issue #4311) I tried failed.
    "Before"="9d3b9202fddb980345025a4f6ac451ed26a423be",    
    "Regression"="752012f577f8e268bb6d0084ca39a09fa7fbc1c4", 
    "Fixed"="9d3b9202fddb980345025a4f6ac451ed26a423be"),
      
  # Test based on https://github.com/Rdatatable/data.table/issues/5424
  # Performance regression introduced in https://github.com/Rdatatable/data.table/pull/4491
  # Fixed in https://github.com/Rdatatable/data.table/pull/5463    
  "Test regression fixed in #5463" = list(
    pkg.edit.fun = pkg.edit.fun,
    N = 10^seq(3, 8),
    expr = quote(data.table:::`[.data.table`(dt_mod, , N := .N, by = g)),
    setup = quote({
      n <- N/100
      set.seed(1L)
      dt <- data.table(
        g = sample(seq_len(n), N, TRUE),
        x = runif(N),
        key = "g")
      dt_mod <- copy(dt)
    }),
    #"Before"="73c221f51c8b545bd5dd06719647aed384a2c4b2", # Previously working, currently fails.
    "Before"="58409197426ced4714af842650b0cc3b9e2cb842",    
    "Regression"="e793f53466d99f86e70fc2611b708ae8c601a451",
    "Fixed"="58409197426ced4714af842650b0cc3b9e2cb842"),

  # Test based on https://github.com/Rdatatable/data.table/issues/4200
  # Performance regression fixed in https://github.com/Rdatatable/data.table/pull/4558
  "Test regression fixed in #4558" = list(
  pkg.edit.fun = pkg.edit.fun,
  N = 10^seq(1, 20),
  expr = quote(data.table:::`[.data.table`(d, , (max(v1) - min(v2)), by = id3)),
  setup = quote({ 
    set.seed(108)
    d <- data.table(
      id3 = sample(c(seq.int(N * 0.9), sample(N * 0.9, N * 0.1, TRUE))),
      v1 = sample(5L, N, TRUE),
      v2 = sample(5L, N, TRUE))
    }),
    "Before" = "15f0598b9828d3af2eb8ddc9b38e0356f42afe4f",
    "Regression" = "6f360be0b2a6cf425f6df751ca9a99ec5d35ed93",
    "Fixed" = "ba32f3cba38ec270587e395f6e6c26a80be36be6")   
)
# Test to see if R is running with --vanilla
# Test to see if R is reading .Rprofile (setting an environment variable and accessing it)
# Debugging more to make the switch to using .Rprofile
# Since the environment variable is being read with the location change of .Rprofile, it is time to test with the CRAN mirror being only set therein
