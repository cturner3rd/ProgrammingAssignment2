## Put comments here that give an overall description of what your
## functions do

## These two functions create the inverse of a matrix and then cache the result.
## We assume that the matrix is invertible.

## Write a short comment describing this function

## This function contains a list of four functions that are supplied as arguments
## to the function cacheSolve. Function cacheSolve performs the actual computation.
## set puts a matrix in cache, making it available to a calling function
## get retrieves a matrix in cache
## setinv stores the result of the inverse operation
## getinv retrieves the result of the inverse operation

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## Write a short comment describing this function

## cacheSolve uses the four functions in makeCacheMatrix to check cache
## for the existence of an inverted matrix. If found, it returns the cached
## version. If not found, it performs the inversion and uses setinv to store
## the result. 
## It uses the function solve to perform the actual inversion

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
