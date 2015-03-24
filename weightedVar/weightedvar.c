#include <R.h>
#include <Rdefines.h>
#include <stdio.h>

SEXP weightedVar(SEXP X, SEXP weights, SEXP center, SEXP flag) {
  SEXP ans;
  
  int i,j;
  weights = coerceVector(weights, REALSXP);
  int len = length(weights); // w and x are of same length
  X = coerceVector(X, REALSXP);
  double *x = REAL(X);
  double w[len];
  for(i=0; i<len; ++i){
    w[i] = REAL(weights)[i]; // can cause overflow for large inputs
  }

  // Standardize weights to sum to one
  double sum = 0.0;
  for(i=0; i<len; ++i){
    sum += w[i]; // can cause overflow for large inputs
  }
  
  for(i=0; i<len; ++i){
    w[i]/=sum;
  }

  // Estimate mean if flag is set else use center
  double c = 0.0;
  if(INTEGER(flag)[0] == 1) {
    for(i=0; i<len; ++i) {
      c += w[i] * x[i];
    }
  }
  else {
    c = REAL(center)[0];
  }

  // Estimate the variance
  PROTECT(ans = allocVector(REALSXP, 1));
  double xt;
  REAL(ans)[0] = 0.0;
  double* an = REAL(ans);
  for (i = 0; i < len; ++i)
  {
    xt = x[i] - c;
    xt = xt * xt;
    an[0] += (w[i] * xt);
  }
  an[0] *= len / (len-1.0);

  UNPROTECT(1);
  return(ans);
}