## This is a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_matrix <<- inverse
  getinverse <- function() inverted_matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by
##  makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  mat <- x$get()
  inverse_matrix <- inverse(mat, ...)
  x$setinverse(inverse_matrix)
  inverse_matrix
}