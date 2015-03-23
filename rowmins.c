#include <R.h>
#include <Rdefines.h>
#include <stdio.h>

SEXP rowMins(SEXP x, SEXP colRange, SEXP dims) {
  SEXP n;
  int i,j;
 
  dims = coerceVector(dims, INTSXP);
  int I = INTEGER (dims)[0];
  int J = INTEGER (dims)[1];
 
  x = coerceVector(x, REALSXP);
  double* X = REAL(x);
 
  colRange = coerceVector(colRange, INTSXP);
  int K = length(colRange);
  int* col = INTEGER(colRange);
 
  PROTECT(n = allocVector(REALSXP, I));
  double* ans = REAL(n);

  for(i = 0; i < I; ++i){
    ans[i] = X[i + I*(col[0] - 1)];
  }

  for(i = 0; i < I; ++i){
    for(j=0;j < K; ++j){
      
      if( ans[i] > X[ i + I*(col[j]-1) ] )
        ans[i] = X[ i + I*(col[j]-1) ];
    }
  }

  UNPROTECT(1);

  return(n);
}
