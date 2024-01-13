#include "data.table.h"

bool within_int32_repres(double x) {
  // N.B. (int)2147483647.99 is not undefined behaviour since s 6.3.1.4 of the C
  // standard states that behaviour is undefined only if the integral part of a
  // finite value of standard floating type cannot be represented.
  return R_FINITE(x) && x < 2147483648 && x > -2147483648;
}

bool within_int64_repres(double x) {
  return R_FINITE(x) && x <= (double)INT64_MAX && x >= (double)INT64_MIN;
}

static R_xlen_t firstNonInt(SEXP x) {
  R_xlen_t n=xlength(x), i=0;
  const double *dx = REAL(x);
  while (i<n &&
         ( ISNA(dx[i]) ||
         (within_int32_repres(dx[i]) && dx[i]==(int)(dx[i])))) {
    i++;
  }
  return i==n ? 0 : i+1;
}

bool isRealReallyInt(SEXP x) {
  return isReal(x) ? firstNonInt(x)==0 : false;
  // used to error if not passed type double but this needed extra is.double() calls in calling R code
  // which needed a repeat of the argument. Hence simpler and more robust to return false when not type double.
}

SEXP isRealReallyIntR(SEXP x) {
  return ScalarLogical(isRealReallyInt(x));
}

SEXP isReallyReal(SEXP x) {
  return ScalarInteger(isReal(x) ? firstNonInt(x) : 0);
  // return the 1-based location of first element which is really real (i.e. not an integer) otherwise 0 (false)
}

bool allNA(SEXP x, bool errorForBadType) {
  // less space and time than all(is.na(x)) at R level because that creates full size is.na(x) first before all()
  // whereas this allNA can often return early on testing the first value without reading the rest
  const int n = length(x);
  if (n==0) // empty vectors (including raw(), NULL, and list()) same as R's all(is.na()) true result; tests 2116.*
    return true;
  switch (TYPEOF(x)) {
  case RAWSXP: // raw doesn't support NA so always false (other than length 0 case above)
    return false;
  case LGLSXP:
  case INTSXP: {
    const int *xd = INTEGER(x);
    for (int i=0; i<n; ++i)    if (xd[i]!=NA_INTEGER) {
      return false;
    }
    return true;
  }
  case REALSXP:
    if (INHERITS(x, char_integer64)) {
      const int64_t *xd = (int64_t *)REAL(x);
      for (int i=0; i<n; ++i)  if (xd[i]!=NA_INTEGER64) {
        return false;
      }
    } else {
      const double *xd = REAL(x);
      for (int i=0; i<n; ++i)  if (!ISNAN(xd[i])) {
        return false;
      }
    }
    return true;
  case CPLXSXP: {
    const Rcomplex *xd = COMPLEX(x);
    for (int i=0; i<n; ++i) if (!ISNAN_COMPLEX(xd[i])) {
      return false;
    }
    return true;
  }
  case STRSXP: {
    const SEXP *xd = STRING_PTR(x);
    for (int i=0; i<n; ++i)    if (xd[i]!=NA_STRING) {
      return false;
    }
    return true;
  }}
  if (!errorForBadType) return false;
  error(_("Unsupported type '%s' passed to allNA()"), type2char(TYPEOF(x)));  // e.g. VECSXP; tests 2116.16-18
  // turned off allNA list support for now to avoid accidentally using it internally where we did not intend; allNA not yet exported
  //   https://github.com/Rdatatable/data.table/pull/3909#discussion_r329065950
}

SEXP allNAR(SEXP x) {
  return ScalarLogical(allNA(x, /*errorForBadType=*/true));
}

/* colnamesInt
 * for provided data.table (or a list-like) and a subset of its columns, it returns integer positions of those columns in DT
 * handle columns input as: integer, double, character and NULL (handled as seq_along(x))
 * adds validation for:
 *   correct range [1,ncol], and if type real checks whole integer
 *   existing columns for character
 *   optionally check for no duplicates
 */
SEXP colnamesInt(SEXP x, SEXP cols, SEXP check_dups) {
  if (!isNewList(x))
    error(_("'x' argument must be data.table compatible"));
  if (!IS_TRUE_OR_FALSE(check_dups))
    error(_("%s must be TRUE or FALSE"), "check_dups");
  int protecti = 0;
  R_len_t nx = length(x);
  R_len_t nc = length(cols);
  SEXP ricols = R_NilValue;
  if (isNull(cols)) { // seq_along(x)
    ricols = PROTECT(allocVector(INTSXP, nx)); protecti++;
    int *icols = INTEGER(ricols);
    for (int i=0; i<nx; i++) icols[i] = i+1;
  } else if (length(cols)==0) { // integer(0)
    ricols = PROTECT(allocVector(INTSXP, 0)); protecti++;
  } else if (isInteger(cols) || isReal(cols)) {
    if (isInteger(cols)) {
      ricols = cols;
    } else if (isReal(cols)) {
      if (!isRealReallyInt(cols))
        error(_("argument specifying columns is type 'double' and one or more items in it are not whole integers"));
      ricols = PROTECT(coerceVector(cols, INTSXP)); protecti++;
    }
    int *icols = INTEGER(ricols);
    for (int i=0; i<nc; i++) {
      if ((icols[i]>nx) || (icols[i]<1))
        error(_("argument specifying columns received non-existing column(s): cols[%d]=%d"), i+1, icols[i]); // handles NAs also
    }
  } else if (isString(cols)) {
    SEXP xnames = PROTECT(getAttrib(x, R_NamesSymbol)); protecti++;
    if (isNull(xnames))
      error(_("'x' argument data.table has no names"));
    ricols = PROTECT(chmatch(cols, xnames, 0)); protecti++;
    int *icols = INTEGER(ricols);
    for (int i=0; i<nc; i++) {
      if (icols[i]==0)
        error(_("argument specifying columns received non-existing column(s): cols[%d]='%s'"), i+1, CHAR(STRING_ELT(cols, i))); // handles NAs also
    }
  } else {
    error(_("argument specifying columns must be character or numeric"));
  }
  if (LOGICAL(check_dups)[0] && any_duplicated(ricols, FALSE))
    error(_("argument specifying columns received duplicate column(s)"));
  UNPROTECT(protecti);
  return ricols;
}

inline bool INHERITS(SEXP x, SEXP char_) {
  // Thread safe inherits() by pre-calling install() in init.c and then
  // passing those char_* in here for simple and fast non-API pointer compare.
  // The thread-safety aspect here is only currently actually needed for list columns in
  // fwrite() where the class of the cell's vector is tested; the class of the column
  // itself is pre-stored by fwrite (for example in isInteger64[] and isITime[]).
  // Thread safe in the limited sense of correct and intended usage :
  // i) no API call such as install() or mkChar() must be passed in.
  // ii) no attrib writes must be possible in other threads.
  SEXP klass;
  if (isString(klass = getAttrib(x, R_ClassSymbol))) {
    for (int i=0; i<LENGTH(klass); ++i) {
      if (STRING_ELT(klass, i) == char_) return true;
    }
    if (char_==char_integer64) {
      // package:nanotime is S4 and inherits from integer64 via S3 extends; i.e. integer64 does not appear in its R_ClassSymbol
      // R's C API inherits() does not cover S4 and returns FALSE for nanotime
      // R's R-level inherits() calls objects.c:inherits2 which calls attrib.c:R_data_class2 and
      // then attrib.c:S4_extends which itself calls R level methods:::.extendsForS3 which then calls R level methods::extends.
      // Since that chain of calls is so complicated and involves evaluating R level (not thread-safe) we
      // special case nanotime here. We used to have Rinherits() as well which did call R level but couldn't be called from
      // parallel regions. That became too hard to reason about two functions, #4752.
      // If any other classes come to light that, like nanotime, S4 inherit from integer64, we can i) encourage them to change
      // to regular S3, or ii) state we simply don't support that; i.e. nanotime was an exception, or iii) add a function that
      // gets called on C entry points which loops through columns and if any are S4 calls the old Rinherits() to see if they S4
      // inherit from integer64, and if so add that class to a vector that gets looped through here. That way we isolate the
      // non-TS call into argument massage header code, and we can continue to use INHERITS() throughout the code base.
      for (int i=0; i<LENGTH(klass); ++i) {
        if (STRING_ELT(klass, i) == char_nanotime) return true;
      }
    }
  }
  return false;
}

SEXP copyAsPlain(SEXP x) {
  // v1.12.2 and before used standard R duplicate() to do this. But duplicate() is not guaranteed to not return an ALTREP.
  // e.g. ALTREP 'wrapper' on factor column (with materialized INTSXP) in package VIM under example(hotdeck)
  //      .Internal(inspect(x[[5]]))
  //      @558adf4d9508 13 INTSXP g0c0 [OBJ,NAM(7),ATT]  wrapper [srt=-2147483648,no_na=0]
  // 'AsPlain' is intended to convey unALTREP-ing; i.e. materializing and removing any ALTREP wrappers/attributes
  // For non-ALTREP this should do the same as R's duplicate().
  // Intended for use on columns; to either un-ALTREP them or duplicate shared memory columns; see copySharedColumns() below
  // Not intended to be called on a DT VECSXP where a concept of 'deep' might refer to whether the columns are copied

  if (isNull(x)) {
    // deal with up front because isNewList(R_NilValue) is true
    return R_NilValue;
  }
  if (!isVectorAtomic(x) && !isNewList(x)) {
    // e.g. defer to R the CLOSXP in test 173.3 where a list column item is the function 'mean'
    return duplicate(x);
  }
  const int64_t n = XLENGTH(x);
  SEXP ans = PROTECT(allocVector(TYPEOF(x), n));
  switch (TYPEOF(x)) {
  case RAWSXP:
    memcpy(RAW(ans),     RAW(x),     n*sizeof(Rbyte));
    break;
  case LGLSXP:
    memcpy(LOGICAL(ans), LOGICAL(x), n*sizeof(Rboolean));
    break;
  case INTSXP:
    memcpy(INTEGER(ans), INTEGER(x), n*sizeof(int));             // covered by 10:1 after test 178
    break;
  case REALSXP:
    memcpy(REAL(ans),    REAL(x),    n*sizeof(double));          // covered by as.Date("2013-01-01")+seq(1,1000,by=10) after test 1075
    break;
  case CPLXSXP:
    memcpy(COMPLEX(ans), COMPLEX(x), n*sizeof(Rcomplex));
    break;
  case STRSXP: {
    const SEXP *xp=STRING_PTR(x);                                // covered by as.character(as.hexmode(1:500)) after test 642
    for (int64_t i=0; i<n; ++i) SET_STRING_ELT(ans, i, xp[i]);
  } break;
  case VECSXP: {
    const SEXP *xp=SEXPPTR_RO(x);
    for (int64_t i=0; i<n; ++i) SET_VECTOR_ELT(ans, i, copyAsPlain(xp[i]));
  } break;
  default:                                                                                           // # nocov
    error(_("Internal error: type '%s' not supported in %s"), type2char(TYPEOF(x)), "copyAsPlain()"); // # nocov
  }
  DUPLICATE_ATTRIB(ans, x);
  // aside: unlike R's duplicate we do not copy truelength here; important for dogroups.c which uses negative truelenth to mark its specials
  if (ALTREP(ans))
    error(_("Internal error: copyAsPlain returning ALTREP for type '%s'"), type2char(TYPEOF(x))); // # nocov
  UNPROTECT(1);
  return ans;
}

void copySharedColumns(SEXP x) {
  const int ncol = length(x);
  if (!isNewList(x) || ncol==1) return;
  bool *shared = (bool *)R_alloc(ncol, sizeof(bool)); // on R heap in case alloc fails
  int *savetl = (int *)R_alloc(ncol, sizeof(int));  // on R heap for convenience but could be a calloc
  const SEXP *xp = SEXPPTR_RO(x);
  // first save the truelength, which may be negative on specials in dogroups, and set to zero; test 2157
  // the savetl() function elsewhere is for CHARSXP. Here, we are using truelength on atomic vectors.
  for (int i=0; i<ncol; ++i) {
    const SEXP thiscol = xp[i];
    savetl[i] = ALTREP(thiscol) ? 0 : TRUELENGTH(thiscol);
    SET_TRUELENGTH(thiscol, 0);
  }
  int nShared=0;
  for (int i=0; i<ncol; ++i) {
    SEXP thiscol = xp[i];
    if (ALTREP(thiscol) || TRUELENGTH(thiscol)<0) {
      shared[i] = true;  // we mark ALTREP as 'shared' too, whereas 'tocopy' would be better word to use for ALTREP
      nShared++;
      // do not copyAsPlain() here yet, as its alloc might fail. Must restore tl first to all columns before attempting any copies.
    } else {
      shared[i] = false;              // so the first column will never be shared (unless it is an altrep) even it is shared
                                      // 'shared' means a later column shares an earlier column
      SET_TRUELENGTH(thiscol, -i-1);  // -i-1 so that if, for example, column 3 shares column 1, in iteration 3 we'll know not
                                      // only that the 3rd column is shared with an earlier column, but which one too. Although
                                      // we don't use that information currently, we could do in future.
    }
  }
  // now we know nShared and which ones they are (if any), restore original tl back to the unique set of columns
  for (int i=0; i<ncol; ++i) {
    if (!shared[i]) SET_TRUELENGTH(xp[i], savetl[i]);
    //  ^^^^^^^^^^ important because if there are shared columns, the dup will have savetl==0 but we want the first restore to stand
  }
  // now that truelength has been restored for all columns, we can finally call copyAsPlain()
  if (nShared) {
    for (int i=0; i<ncol; ++i) {
      if (shared[i])
        SET_VECTOR_ELT(x, i, copyAsPlain(xp[i]));
    }
    if (GetVerbose()) Rprintf(_("Found and copied %d column%s with a shared memory address\n"), nShared, nShared>1?"s":"");
    // GetVerbose() (slightly expensive call of all options) called here only when needed
  }
}

// lock, unlock and islocked at C level :
// 1) for speed to reduce overhead
// 2) to avoid an R level wrapper which bumps MAYBE_SHARED; see the unlock after eval(jval) in data.table.R, #1341 #2245
SEXP lock(SEXP DT) {
  setAttrib(DT, sym_datatable_locked, ScalarLogical(TRUE));
  return DT;
}
SEXP unlock(SEXP DT) {
  setAttrib(DT, sym_datatable_locked, R_NilValue);
  return DT;
}
bool islocked(SEXP DT) {
  SEXP att = getAttrib(DT, sym_datatable_locked);
  return isLogical(att) && LENGTH(att)==1 && LOGICAL(att)[0]==1;
}
SEXP islockedR(SEXP DT) {
  return ScalarLogical(islocked(DT));
}

bool need2utf8(SEXP x) {
  const int xlen = length(x);
  SEXP *xd = STRING_PTR(x);
  for (int i=0; i<xlen; i++) {
    if (NEED2UTF8(xd[i]))
      return(true);
  }
  return(false);
}

SEXP coerceUtf8IfNeeded(SEXP x) {
  if (!need2utf8(x))
    return(x);
  const int xlen = length(x);
  SEXP ans = PROTECT(allocVector(STRSXP, xlen));
  SEXP *xd = STRING_PTR(x);
  for (int i=0; i<xlen; i++) {
    SET_STRING_ELT(ans, i, ENC2UTF8(xd[i]));
  }
  UNPROTECT(1);
  return(ans);
}

// class1 is used by coerseAs only, which is used by frollR.c and nafill.c only
const char *class1(SEXP x) {
  SEXP cl = getAttrib(x, R_ClassSymbol);
  if (length(cl))
    return(CHAR(STRING_ELT(cl, 0)));
  SEXP d = getAttrib(x, R_DimSymbol);
  int nd = length(d);
  if (nd) {
    if (nd==2)
      return "matrix";
    else
      return "array";
  }
  SEXPTYPE t = TYPEOF(x);
  // see TypeTable in src/main/utils.c to compare to the differences here vs type2char
  switch(t) {
  case CLOSXP: case SPECIALSXP: case BUILTINSXP:
    return "function";
  case REALSXP:
    return "numeric";
  case SYMSXP:
    return "name";
  case LANGSXP:
    return "call";
  default:
    return type2char(t);
  }
}

// main motivation for this function is to have coercion helper that is aware of int64 NAs, unline base R coerce #3913
SEXP coerceAs(SEXP x, SEXP as, SEXP copyArg) {
  // copyArg does not update in place, but only IF an object is of the same type-class as class to be coerced, it will return with no copy
  if (!isVectorAtomic(x))
    error(_("'x' is not atomic"));
  if (!isNull(getAttrib(x, R_DimSymbol)))
    error(_("'x' must not be matrix or array"));
  if (!isNull(getAttrib(as, R_DimSymbol)))
    error(_("input must not be matrix or array"));
  bool verbose = GetVerbose()>=2; // verbose level 2 required
  if (!LOGICAL(copyArg)[0] && TYPEOF(x)==TYPEOF(as) && class1(x)==class1(as)) {
    if (verbose)
      Rprintf(_("copy=false and input already of expected type and class %s[%s]\n"), type2char(TYPEOF(x)), class1(x));
    copyMostAttrib(as, x); // so attrs like factor levels are same for copy=T|F
    return(x);
  }
  int len = LENGTH(x);
  SEXP ans = PROTECT(allocNAVectorLike(as, len));
  if (verbose)
    Rprintf(_("Coercing %s[%s] into %s[%s]\n"), type2char(TYPEOF(x)), class1(x), type2char(TYPEOF(as)), class1(as));
  const char *ret = memrecycle(/*target=*/ans, /*where=*/R_NilValue, /*start=*/0, /*len=*/LENGTH(x), /*source=*/x, /*sourceStart=*/0, /*sourceLen=*/-1, /*colnum=*/0, /*colname=*/"");
  if (ret)
    warning(_("%s"), ret);
  UNPROTECT(1);
  return ans;
}

#ifndef NOZLIB
#include <zlib.h>
#endif
SEXP dt_zlib_version(void) {
  char out[71];
#ifndef NOZLIB
  snprintf(out, 70, "zlibVersion()==%s ZLIB_VERSION==%s", zlibVersion(), ZLIB_VERSION);
#else
  snprintf(out, 70, _("zlib header files were not found when data.table was compiled"));
#endif
  return ScalarString(mkChar(out));
}
SEXP dt_has_zlib(void) {
#ifndef NOZLIB
  return ScalarLogical(1);
#else
  return ScalarLogical(0);
#endif
}

SEXP startsWithAny(const SEXP x, const SEXP y, SEXP start) {
  // for is_url in fread.R added in #5097
  // startsWith was added to R in 3.3.0 so we need something to support R 3.1.0
  // short and simple ascii-only
  if (!isString(x) || !isString(y) || length(x)!=1 || length(y)<1 || !isLogical(start) || length(start)!=1 || LOGICAL(start)[0]==NA_LOGICAL)
    error("Internal error: data.table's internal startsWithAny types or lengths incorrect");
  const char *xd = CHAR(STRING_ELT(x, 0));
  const int n=length(y);
  if (LOGICAL(start)[0]) {
    for (int i=0; i<n; ++i) {
      const char *yd = CHAR(STRING_ELT(y, i));
      if (strncmp(xd, yd, strlen(yd))==0)
        return ScalarInteger(i+1);
    }
  } else {
    const int xlen = strlen(xd);
    for (int i=0; i<n; ++i) {
      const char *yd = CHAR(STRING_ELT(y, i));
      const int ylen=strlen(yd);
      if (xlen>=ylen && strncmp(xd+xlen-ylen, yd, ylen)==0)
        return ScalarInteger(i+1);
    }
  }
  return ScalarLogical(false);
}

SEXP frev(SEXP x, SEXP copyArg) {
  int n = LENGTH(x);
  if (!LOGICAL(copyArg)[0]) {
    if (n==0) return x;
    switch (TYPEOF(x)) {
      case LGLSXP: case INTSXP: {
        int *restrict xd = INTEGER(x);
        #pragma omp parallel for num_threads(getDTthreads(n, true))
        for (uint64_t i=0; i<n/2; ++i) {
          const int k = n-1-i;
          const int tmp = xd[i];
          xd[i] = xd[k];
          xd[k] = tmp;
        }
      } break;
      case REALSXP: if (INHERITS(x, char_integer64)) {
        int64_t *xd = (int64_t *)REAL(x);
        #pragma omp parallel for num_threads(getDTthreads(n, true))
        for (uint64_t i=0; i<n/2; ++i) {
          const int k = n-1-i;
          const int64_t tmp = xd[i];
          xd[i] = xd[k];
          xd[k] = tmp;
        }
      } else {
        double *xd = REAL(x);
        #pragma omp parallel for num_threads(getDTthreads(n, true))
        for (uint64_t i=0; i<n/2; ++i) {
          const int k = n-1-i;
          const double tmp = xd[i];
          xd[i] = xd[k];
          xd[k] = tmp;
        }
      } break;
      case STRSXP: {
        #pragma omp parallel for num_threads(getDTthreads(n, true))
        for (uint64_t i=0; i<n/2; ++i) {
          const int k = n-1-i;
          const SEXP tmp = STRING_ELT(x, i);
          SET_STRING_ELT(x, i, STRING_ELT(x, k));
          SET_STRING_ELT(x, k, tmp);
        }
      } break;
      case VECSXP: {
        #pragma omp parallel for num_threads(getDTthreads(n, true))
        for (uint64_t i=0; i<n/2; ++i) {
          const int k = n-1-i;
          const SEXP tmp = VECTOR_ELT(x, i);
          SET_VECTOR_ELT(x, i, VECTOR_ELT(x, k));
          SET_VECTOR_ELT(x, k, tmp);
        }
      } break;
      case CPLXSXP: {
        Rcomplex *xd = COMPLEX(x);
        #pragma omp parallel for num_threads(getDTthreads(n, true))
        for (uint64_t i=0; i<n/2; ++i) {
          const int k = n-1-i;
          const Rcomplex tmp = xd[i];
          xd[i] = xd[k];
          xd[k] = tmp;
        }
      } break;
      case RAWSXP: {
        Rbyte *xd = RAW(x);
        #pragma omp parallel for num_threads(getDTthreads(n, true))
        for (uint64_t i=0; i<n/2; ++i) {
          const int k = n-1-i;
          const Rbyte tmp = xd[i];
          xd[i] = xd[k];
          xd[k] = tmp;
        }
      } break;
    default:
      error(_("Type '%s' is not supported"), type2char(TYPEOF(x)));
    }
    return x;
  } else {
    SEXP ans = PROTECT(allocVector(TYPEOF(x), n));
    if (n==0) {
      UNPROTECT(1);
      return ans;
    }
    const bool factor = isFactor(x);
    switch (TYPEOF(x)) {
      case LGLSXP: case INTSXP: {
        int *restrict ansd = INTEGER(ans);
        const int *restrict xd = INTEGER(x);
        #pragma omp parallel for num_threads(getDTthreads(n, true))
        for (uint64_t i=0; i<n; ++i) {
          ansd[n-1-i]=xd[i];
        }
      } break;
      case REALSXP: { if (INHERITS(x, char_integer64)) {
        int64_t *restrict ansd = (int64_t *)REAL(ans);
        const int64_t *restrict xd = (const int64_t *)REAL(x);
        #pragma omp parallel for num_threads(getDTthreads(n, true))
        for (uint64_t i=0; i<n; ++i) {
          ansd[n-1-i]=xd[i];
        }
      } else {
        double *restrict ansd = REAL(ans);
        const double *restrict xd = REAL(x);
        #pragma omp parallel for num_threads(getDTthreads(n, true))
        for (uint64_t i=0; i<n; ++i) {
          ansd[n-1-i]=xd[i];
        }
      }} break;
      case STRSXP: {
        #pragma omp parallel for num_threads(getDTthreads(n, true))
        for (uint64_t i=0; i<n; ++i) {
          SET_STRING_ELT(ans, n-1-i, STRING_ELT(x, i));
        }
      } break;
      case VECSXP: {
        #pragma omp parallel for num_threads(getDTthreads(n, true))
        for (uint64_t i=0; i<n; ++i) {
          SET_VECTOR_ELT(ans, n-1-i, VECTOR_ELT(x, i));
        }
      } break;
      case CPLXSXP: {
        Rcomplex *restrict ansd = COMPLEX(ans);
        const Rcomplex *restrict xd = COMPLEX(x);
        #pragma omp parallel for num_threads(getDTthreads(n, true))
        for (uint64_t i=0; i<n; ++i) {
          ansd[n-1-i]=xd[i];
        }
      } break;
      case RAWSXP: {
        Rbyte *restrict ansd = RAW(ans);
        const Rbyte *restrict xd = RAW(x);
        #pragma omp parallel for num_threads(getDTthreads(n, true))
        for (uint64_t i=0; i<n; ++i) {
          ansd[n-1-i]=xd[i];
        }
      } break;
      default:
        error(_("Type '%s' is not supported"), type2char(TYPEOF(x)));
    }
    copyMostAttrib(x, ans);
    UNPROTECT(1);
    return ans;
  }
}
