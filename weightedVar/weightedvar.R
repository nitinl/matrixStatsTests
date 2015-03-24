weightedVar <- function(x, w, na.rm=FALSE, center=NULL, ...){
  
  # Keeping error checks and prior optimizations same as original
  
  # Argument 'x':
  n <- length(x);

  # Argument 'w':
  if (missing(w)) {
    # By default use weights that are one.
    w <- rep(1, times=n);
  } else if (length(w) != n) {
    stop("The number of elements in arguments 'w' and 'x' does not match: ", length(w), " != ", n);
  }

  # Argument 'na.rm':


  naValue <- NA;
  storage.mode(naValue) <- storage.mode(x);


  # Remove values with zero (and negative) weight. This will:
  #  1) take care of the case when all weights are zero,
  #  2) it will most likely speed up the sorting.
  tmp <- (w > 0);
  if (!all(tmp)) {
    x <- .subset(x, tmp);
    w <- .subset(w, tmp);
    n <- length(x);
  }
  tmp <- NULL; # Not needed anymore

  # Drop missing values?
  dyn.load("anyMissing.so");
  missing <- .Call("anyMissing",x)
  if (na.rm) {
    keep <- which(!is.na(x) & !is.na(w));
    x <- .subset(x, keep);
    w <- .subset(w, keep);
    n <- length(x);
    keep <- NULL; # Not needed anymore
  } else if (missing) {
    return(naValue);
  }

  # Are any weights Inf? Then treat them with equal weight and all others
  # with weight zero.
  tmp <- is.infinite(w);
  if (any(tmp)) {
    keep <- tmp;
    x <- .subset(x, keep);
    n <- length(x);
    w <- rep(1, times=n);
    keep <- NULL; # Not needed anymore
  }
  tmp <- NULL; # Not needed anymore

  
  # Are there any values left to calculate the weighted median of?
  # This is consistent with how stats::mad() works.
  if (n == 0L) {
    return(naValue);
  } else if (n == 1L) {
    zeroValue <- 0;
    storage.mode(zeroValue) <- storage.mode(x);
    return(zeroValue);
  }
#done with error checks and prior optimizations
  
  flag = 0
  if(is.null(center)) flag = 1

# computationally intensive part starts here
  dyn.load("weightedvar.so")
  n <- .Call("weightedVar", x, w, center, as.integer(flag))
  n;
}