#include <R.h>
#include <Rdefines.h>
#include <stdio.h>


SEXP anyMissing(SEXP x) {
  SEXP ans;
  R_xlen_t nx, ii;
  double *xdp;
  int *xip, *xlp;
  Rcomplex *xcp;

  PROTECT(ans = allocVector(LGLSXP, 1));
  LOGICAL(ans)[0] = 0;

  nx = xlength(x);

  /* anyMissing() on zero-length objects should always return FALSE,
     just like any(double(0)). */
  if (nx == 0) {
    UNPROTECT(1);
    return(ans);
  }

  switch (TYPEOF(x)) {
    case REALSXP:
      xdp = REAL(x);
      for (ii=0; ii < nx; ii++) {
        if ISNAN(xdp[ii]) {
          LOGICAL(ans)[0] = 1;
          break;
        }
      }
      break;

    case INTSXP:
      xip = INTEGER(x);
      for (ii=0; ii < nx; ii++) {
        if (xip[ii] == NA_INTEGER) {
          LOGICAL(ans)[0] = 1;
          break;
        }
      }
      break;

    default:
      break;
  } /* switch() */

  UNPROTECT(1); /* ans */

  return(ans);
} // anyMissing()