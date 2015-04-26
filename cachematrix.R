## The following functions allow the user to cache the inverse of a matrix,
## in order to save costly computation when the inverse of the same matrix is
## needed repeatedly, e.g. in a loop.
## Note that the functions do not contain tests for the invertibility of the 
## matrix supplied, it is assumed, that it is always invertible!

## This function creates a special "matrix" object that can cache its inverse.
## It creates a list of functions for setting a new matrix (and deleting the 
## cached inverse), getting the matrix, setting and getting the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    ## assigns y to x in the parent environment
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of the matrix 'x'. If the inverse for 'x'
## was already computed, returns the cached inverse, otherwise computes the 
## inverse and returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
