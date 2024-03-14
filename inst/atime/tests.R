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
  # Test based on https://github.com/Rdatatable/data.table/issues/5424
  # Performance regression introduced in: https://github.com/Rdatatable/data.table/pull/4491
  "Test regression introduced from #4491" = list(
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
    "Before"="be2f72e6f5c90622fe72e1c315ca05769a9dc854",
    "Regression"="e793f53466d99f86e70fc2611b708ae8c601a451",
    "Fixed"="58409197426ced4714af842650b0cc3b9e2cb842")
  )
