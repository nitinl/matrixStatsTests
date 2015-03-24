#######################################################################
#
# Tests for weightedVar. Slightly modified unit test for weightedVar
# from matrixStats library. Also contains original weightedVar
# function renamed as weightedVar_R
#
# Original weightedVar function has modified only at anyMissing, 
# loading anyMissing function from .so file.
# 
# anyMissing has been changed to test only for real and integer types
#
#######################################################################

weightedVar_R <- function(x, w, na.rm=FALSE, center=NULL, ...) {
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
  
  dyn.load("anyMissing.so");
  anyMissing <- .Call("anyMissing",x)
  # Drop missing values?
  if (na.rm) {
    keep <- which(!is.na(x) & !is.na(w));
    x <- .subset(x, keep);
    w <- .subset(w, keep);
    n <- length(x);
    keep <- NULL; # Not needed anymore
  } else if (anyMissing) {
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
  
  # Standardize weights to sum to one
  w <- w / sum(w);
  
  # Estimate the mean?
  if (is.null(center)) {
    center <- sum(w*x);
  }
  
  # Estimate the variance
  x <- x - center; # Residuals
  x <- x^2;        # Squared residuals
  sigma2 <- sum(w*x) * (n / (n-1L))
  
  x <- w <- NULL; # Not needed anymore
  
  sigma2;
} # weightedVar()



# TESTING, output of C implementation and original function
# shown one after other for quick comparison

for (mode in c("integer", "double")) {
  cat("mode: ", mode, "\n", sep="")
  n <- 15L
  x <- runif(n, min=-5, max=5)
  storage.mode(x) <- mode
  str(x)
  
  for (addNA in c(FALSE, TRUE)) {
    cat("addNA: ", addNA, "\n", sep="")
    if (addNA) {
      x[c(5,7)] <- NA
    }
    str(x)
    
    for (na.rm in c(FALSE, TRUE)) {
      cat("na.rm: ", na.rm, "\n", sep="")
      
      cat("Weights not specified\n")
      w <- rep(1, times=n)
      m1 <- weightedVar_R(x, na.rm=na.rm)
      cat('Original:')
      str(list(m1=m1))
      m1 <- weightedVar(x, na.rm=na.rm)
      cat('C implementation:')
      str(list(m1=m1))
      
      cat("All weights are 1\n")
      w <- rep(1, times=n)
      m1 <- weightedVar_R(x, w, na.rm=na.rm)
      cat('Original:')
      str(list(m1=m1))
      m1 <- weightedVar(x, w, na.rm=na.rm)
      cat('C implementation:')
      str(list(m1=m1))
      
      cat("First weight is 5\n")
      # Pull the mean towards zero
      w[1] <- 5
      m1 <- weightedVar_R(x, w, na.rm=na.rm)
      cat('Original:')
      str(list(m1=m1))
      m1 <- weightedVar(x, w, na.rm=na.rm)
      cat('C implementation:')
      str(list(m1=m1))
      
      cat("All weights are 0\n")
      # All weights set to zero
      w <- rep(0, times=n)
      m1 <- weightedVar_R(x, w, na.rm=na.rm)
      cat('Original:')
      str(list(m1=m1))
      m1 <- weightedVar(x, w, na.rm=na.rm)
      cat('C implementation:')
      str(list(m1=m1))
      
      cat("First weight is 8.5\n")
      # Put even more weight on the zero
      w <- rep(1, times=n)
      w[1] <- 8.5
      m1 <- weightedVar_R(x, w, na.rm=na.rm)
      cat('Original:')
      str(list(m1=m1))
      m1 <- weightedVar(x, w, na.rm=na.rm)
      cat('C implementation:')
      str(list(m1=m1))
      
      cat("First weight is Inf\n")
      # All weight on the first value
      w[1] <- Inf
      m1 <- weightedVar_R(x, w, na.rm=na.rm)
      cat('Original:')
      str(list(m1=m1))
      m1 <- weightedVar(x, w, na.rm=na.rm)
      cat('C implementation:')
      str(list(m1=m1))
      
      cat("Last weight is Inf\n")
      # All weight on the last value
      w[1] <- 1
      w[n] <- Inf
      m1 <- weightedVar_R(x, w, na.rm=na.rm)
      cat('Original:')
      str(list(m1=m1))
      m1 <- weightedVar(x, w, na.rm=na.rm)
      cat('C implementation:')
      str(list(m1=m1))
    } # for (na.rm ...)
  } # for (addNA ...)
} # for (mode ...)

cat("DONE\n")