## 
## How to use
##
## First create a matrix: 
##    matrix00 <- cbind(c(1,2), c(3,4))
##
## Then make a special matrix from the one you create:
##    spcmatrix00 <- makeCacheMatrix(matrix00)
##
## Finally, get the inverse:
##    inv00 <- cacheSolve(spcmatrix00)
##

## ---------------------------------------------------------------------------

## This function creates a list with four functions that are to be applied in
## the matrix passed as argument
## Function setmatrix caches the matrix
## Function getmatrix returns the matrix
## Function setinv caches the inverse
## Function getinv returns the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  setinv <- function(solved) inv <<- solved
  getinv <- function() inv
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinv = setinv, getinv = getinv)
}


## This function returns the inverse of a matrix generated with the 
## makeCacheMatrix function
## It first verifies if the matrix already has an inverse cached by calling getinv
## If not, then it computes the inverse by calling solve
## and caches it by calling setinv

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  mat <- x$getmatrix()
  inv <- solve(mat)
  x$setinv(inv)
  inv
}





