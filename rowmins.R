rowMins_C <- function(x, cols = 1:ncol(x)){
  if(!is.matrix(x)){
    stop('Argument x must be a matrix')
  }
  if(!is.vector(cols)){
    stop('Argument cols must be a vector')
  }
  if(cols[1] > ncol(x) || cols[1] < 1 || cols[length(cols)] > ncol(x) || cols[length(cols)] < 1){
    stop('Index out of bounds')
  }
  dims = as.integer(dim(x))
  #change path to location of .so file
  dyn.load("rowmins.so")
  n <- .Call("rowMins", x, cols, dims)
  n;
}