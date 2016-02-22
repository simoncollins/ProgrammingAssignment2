## These functions allow you to create a wrapper
## around R's standard matrix object and to use
## this wrapper to cache the value of the matrix's inverse
##
## Example usage:
##   myMatrix <- matrix(c(4,5,92,11,51,12,35,44,7),3,3)
##   wrapped <- makeCacheMatrix(myMatrix)
##   cacheSolve(wrapped)
##   wrapped$set(myOtherMatrix)

## This function takes a R matrix object
## and returns a wrapped object with methods to
## allow the matrix's inverse to be cached for reuse
## Parameters:
##  currMatrix The matrix to initialise the wrapper with

makeCacheMatrix <- function(currMatrix = matrix()) {
  inv <- NULL
  set <- function(newMatrix) {
    # Update the current matrix
    currMatrix <<- newMatrix
    
    # Clear cached inverse when a new matrix is set
    inv <<- NULL
  }
  get <- function() currMatrix
  setInverse <- function(i) inv <<- i
  getInverse <- function() inv
  
  # Return our matrix wrapper object
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function takes a wrapped matrix object
## created by makeCacheMatrix and solves it for its inverse
## making use of the cached version if found.
## Parameters:
##   mx the matrix to solve for the inverse (assumed to be invertible)

cacheSolve <- function(mx) {
  inv <- mx$getInverse()
  # Check for cached inverse
  if(!is.null(inv)) {
    message("using precomputed inverse")
    return(inv)
  }
  # not cached - compute, cache and return
  data <- mx$get()
  inv <- solve(data)
  mx$setInverse(inv)
  inv
}
