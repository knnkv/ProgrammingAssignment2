## For the purpose of saving the computational resources and time
## we want to store the results of our calculations in cache
## so that we don't need to recompute the values every time
## we need to access these values.
## This is especially crucial when calculations are complex
## and require significant time to be completed

## makeCacheMatrix creates a list with 4 functions:
## 1. set(x) assigns a new matrix
## 2. get() returns the matrix from cache
## 3. setinv(x) assigns the solved matrix
## 4. getinv() returns the solved matrix from cache
 
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set=set,get=get,
       setinv=setinv, getinv=getinv)
}


## cacheSolve is a function that 'solves' the matrix from the previous function or inverts it.
## It checks whether the inverted matrix already exists: if yes, it simply returns it from cache
## if not - solves the matrix and saves the result in cache.

cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data)
  x$setinv(xinv)
  xinv
}
