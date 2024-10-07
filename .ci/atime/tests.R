# Test case adapted from https://github.com/Rdatatable/data.table/issues/6105#issue-2268691745
extra.args.6107 <- c(
  "colClasses=list(Date='date')",
  "colClasses='Date'",
  "select=list(Date='date')"
)
extra.test.list <- list()

for (extra.arg in extra.args.6107){
  this.test <- atime::atime_test(
    setup = {
      set.seed(1)
      DT = data.table(date=.Date(sample(20000, N, replace=TRUE)))
      tmp_csv = tempfile()
      fwrite(DT, tmp_csv)
    },
    Slow = "e9087ce9860bac77c51467b19e92cf4b72ca78c7", 
    Fast = "a77e8c22e44e904835d7b34b047df2eff069d1f2"
  )
  this.test$expr <- str2lang(sprintf("data.table::fread(tmp_csv, %s)", extra.arg))
  extra.test.list[[sprintf("fread(%s) improved in #6107", extra.arg)]] <- this.test
}

# Test case adapted from https://github.com/Rdatatable/data.table/pull/4386#issue-602528139
retGrp_values <- c("T","F")
for(retGrp_setup in retGrp_values){
  for(retGrp_expr in retGrp_values){
    test.name <- sprintf("forderv(retGrp=%s-%s) improved in #4386", retGrp_setup, retGrp_expr)
    extra.test.list[[test.name]] <- list(
      setup = quote({
        options(datatable.forder.auto.index = TRUE)
        set.seed(1)
        dt <- data.table(index = sample(N), values = sample(N))
        index.list <- list()
        for(retGrp in retGrp_values){
          data.table:::forderv(dt, "index", retGrp = eval(str2lang(retGrp)))
          index.list[[retGrp]] <- attr(dt, "index")
        }
      }),
      expr = substitute({
        setattr(dt, "index", index.list[[retGrp_setup]]) 
        data.table:::forderv(dt, "index", retGrp = retGrp_expr) # Reusing the index and computing group info.
      }, list(
        retGrp_setup = retGrp_setup,
        retGrp_expr = retGrp_expr
      )),
      Slow = "c152ced0e5799acee1589910c69c1a2c6586b95d", 
      Fast = "1a84514f6d20ff1f9cc614ea9b92ccdee5541506"
    )
  }
}

# A list of performance tests.
test.list <- atime::atime_test_list(
  # Common N and pkg.edit.fun are defined here
  N = as.integer(10^seq(1, 7, by=0.25)),
  pkg.edit.fun = function(old.Package, new.Package, sha, new.pkg.path) {
    # Function body remains unchanged
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

  # Existing tests with consistent expr definitions
  "shallow regression fixed in #4440" = atime::atime_test(
    setup = {
      set.seed(1L)
      dt <- data.table(a = sample.int(N))
      setindexv(dt, "a")
    },
    expr = quote(data.table:::shallow(dt)),
    Regression = "b1b1832b0d2d4032b46477d9fe6efb15006664f4",
    Fixed = "9d3b9202fddb980345025a4f6ac451ed26a423be"
  ),

  "memrecycle regression fixed in #5463" = atime::atime_test(
    setup = {
      bigN <- N*100
      set.seed(2L)
      dt <- data.table(
        g = sample(seq_len(N), bigN, TRUE),
        x = runif(bigN),
        key = "g")
      dt_mod <- copy(dt)
    },
    expr = quote(data.table:::`[.data.table`(dt_mod, , N := .N, by = g)),
    Before = "be2f72e6f5c90622fe72e1c315ca05769a9dc854",
    Regression = "e793f53466d99acee1589910c69c1a2c6586b95d",
    Fixed = "58409197426ced4714af842650b0cc3b9e2cb842"
  ),

  "setDT improved in #5427" = atime::atime_test(
    setup = {
      L <- replicate(N, 1, simplify = FALSE)
      setDT(L)
    },
    expr = quote({
      data.table:::setattr(L, "class", NULL)
      data.table:::setDT(L)
    }),
    Slow = "c4a2085e35689a108d67dacb2f8261e4964d7e12",
    Fast = "af48a805e7a5026a0c2d0a7fd9b587fea5cfa3c4"
  ),

  "DT[by] fixed in #4558" = atime::atime_test(
    setup = {
      d <- data.table(
        id = sample(c(seq.int(N * 0.9), sample(N * 0.9, N * 0.1, TRUE))),
        v1 = sample(5L, N, TRUE),
        v2 = sample(5L, N, TRUE)
      )
    },
    expr = quote(data.table:::`[.data.table`(d, , max(v1) - min(v2), by = id)),
    Before = "7a9eaf62ede487625200981018d8692be8c6f134",
    Regression = "c152ced0e5799acee1589910c69c1a2c6586b95d",
    Fixed = "f750448a2efcd258b3aba57136ee6a95ce56b302"
  ),

  "DT[,.SD] improved in #4501" = atime::atime_test(
    setup = {
      set.seed(1)
      L = as.data.table(as.character(rnorm(N, 1, 0.5)))
      setkey(L, V1)
    },
    expr = quote(data.table:::`[.data.table`(L, , .SD)),
    Fast = "353dc7a6b66563b61e44b2fa0d7b73a0f97ca461",
    Slow = "3ca83738d70d5597d9e168077f3768e32569c790"
    # Removed 'Slower' label if not supported
  ),

  "DT[by,verbose=TRUE] improved in #6296" = atime::atime_test(
    setup = {
      dt = data.table(a = 1:N)
      dt_mod <- copy(dt)
    },
    expr = quote(data.table:::`[.data.table`(dt_mod, , 1, by = a, verbose = TRUE)),
    Slow = "a01f00f7438daf4612280d6886e6929fa8c8f76e",
    Fast = "f248bbe6d1204dfc8def62328788eaadcc8e17a1"
  ),

  "transform improved in #5493" = atime::atime_test(
    setup = {
      df <- data.frame(x = runif(N))
      dt <- as.data.table(df)
    },
    expr = quote(data.table:::transform.data.table(dt, y = round(x))),
    Slow = "0895fa247afcf6b38044bd5f56c0d209691ddb31",
    Fast = "2d1a0575f87cc50e90f64825c30d7a6cb6b05dd7"
  ),

  "melt improved in #5054" = atime::atime_test(
    setup = {
      DT <- as.data.table(as.list(1:N))
      measure.vars <- lapply(1:N, function(i) {
        x = rep(NA, N)
        x[i] = i
        x
      })  
    },
    expr = quote(data.table:::melt(DT, measure.vars = measure.vars)),
    Slow = "fd24a3105953f7785ea7414678ed8e04524e6955",
    Fast = "ed72e398df76a0fcfd134a4ad92356690e4210ea"
  ),

  tests = extra.test.list # Appending the extra tests (fread and forderv atm)
)
# nolint end: undesirable_operator_linter.
