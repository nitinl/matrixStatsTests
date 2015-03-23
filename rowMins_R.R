rowMins_R <- function(x, cols = 1:ncol(x)){
  
  if(!is.matrix(x)){
    stop('Argument x must be a matrix')
  }
  if(!is.vector(cols)){
    stop('Argument cols must be a vector')
  }
  if(cols[1] > ncol(x) || cols[length(cols)] > ncol(x)){
    stop('Index out of bounds')
  }
  n = x[,cols[1]] # n is vector storing result
  flag = 0
  for (i in 1:nrow(x)) {
    for (j in cols) {
      # Warn if non numeric items present
      if (!is.numeric(x[i,j])) {
        flag = 1
      }
      if (x[i,j] < n[i]) {
        n[i] = x[i,j]
      }
    }
  }
  if (flag == 1) {
    warning('Non numeric values present in x')
  }
  n;
}
