## Put comments here that give an overall description of what your
## functions do
## The functions are used to find the inverse of any mxn matrix.
## The function will cache the matrix and return the result from
## the cached copy if the user requests it again.

## Write a short comment describing this function
## this function is used to store the matrix and it's inverse.
## We can also retrieve the stored matrix by using get and getMatrixInverse inner functions.
makeCacheMatrix <- function(x = matrix()) {
  #Free Variables
  cached_i <- NULL
  
  setMatrixInverse <- function(matrixInverse) {
    cached_i <<- matrixInverse
  } 
  
  getMatrixInverse <- function() {
    return(cached_i)
  }
  
  set <- function(y) {
    x <<- y
    cached_i <<- NULL
  }
  
  get <- function() {
    return(x)
  }
  
  list(set = set,
       get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## Write a short comment describing this function
## The function cacheSolve() returns the inverse of the matrix.
## If the matrix is already present, it instead returns
## from the cachedcopy that we stored.

cacheSolve <- function(x, ...) {
  cached_i <- x$getMatrixInverse()
  if (!is.null(cached_i)) {
    message("Retriving cached copy.")
    return(cached_i)
  }
  print(x$get())
  c <- x$get()
  cached_i <- solve(c, ...)
  x$setMatrixInverse(cached_i)
  return(cached_i)
}
