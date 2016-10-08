## These two functions work together to return the inverse of a matrix,
## either through computation or from a cached result if we've already
## computed the inverse.

## 'makeCacheMatrix' takes an invertible matrix and creates a list
## to eventually pass to 'cacheSolve'

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## 'cacheSolve' takes a saved list (created by makeCacheMatrix)
## and returns the inverse of the matrix.
## If we've already calculated the inverse, function returns the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
