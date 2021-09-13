## Put comments here that give an overall description of what your
## functions do

## These functions cache the inverse of the matrix.
## This function creates an object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y #assigns value to object in environment different from current environment
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function gets the inverse of the object returned.

cacheSolving <- function(x, ...) {
  ## Return matrix inverse of x
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting Cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
