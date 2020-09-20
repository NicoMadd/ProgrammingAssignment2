
## makeCacheMatrix
## The function creates a list with the functions get, set, getInverse, setInverse
## in order to make it compatible with the cacheSolve function under it, so it can
## be cached and dont having to process it again if it has been done recently.
##
## i: inverse matrix in the function environment
## x: matrix input
## m: matrix input to set function
## invMatrix: invMatrix input already solved to be set
##
## function returns a list with said functions.

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(m){
    x <<- m
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(invMatrix) i <<- invMatrix
  getInverse <- function() i
  list(set = set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## cacheSolve
## The function solves a formatted matrix with makeCacheMatrix. First checks if
## the matrix has been cached or processed before, if it has, then it returns it
## without making any computations. If it hasn't then it solves it and sets it
## (caches it) so it can be obtained later.
##
## i: inverse matrix to return
## data: setted matrix

## function returns an inverse matrix.

cacheSolve <- function(x, ...){
  
  i <- x$getInverse()
  if(!is.null(i)){
    message("Returning cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data,...)
  x$setInverse(i)
  message("Returning calculated inverse matrix")
  i
}
