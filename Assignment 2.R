makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## ASSIGNMENT: Caching the Inverse of a Matrix

## The following functions make a special object containing a matrix
## and then cache its inverse. 

## The function, "makeCacheMatrix", makes a special object ("matrix") 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse)
}

## The function, "cacheSolve", takes the special matrix from the 
## function above and makes its inverse from its cache, as long as
## the inverse was already calculated and the matrix stayed the same.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()              
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }                            ## Return matrix that is inverse of x
  ok <- x$get()
  inv <- solve(ok, ...)
  x$setInverse(inv)
  inv
}