## In this assignment we work with the <<- operator which can be used to assign
## a value to an object in an environment that is different from the current
## environment. Below are two functions that are used to create a special
## object that stores a numeric matrix and cache's its inverse 

## The makeCacheMatrix function creates a special "matrix", which is really a
## list containing functions to
##    1) set the value of the matrix
##    2) get the value of the matrix
##    3) set the value of the inverse of the matrix
##    4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the function "makeCacheMatrix". However, it first checks to see
## if the inverse has already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the setinverse
## function.

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
