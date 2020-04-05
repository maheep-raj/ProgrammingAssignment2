## Put comments here that give an overall description of what your
## functions do
## A pair of functions that cache the inverse of a matrix

##matrix object that can cache inverse
makeCacheMatrix <- function( m = matrix() ) {
  i <- NULL
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  get <- function() {
    m
  }
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() {
    i
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Compute theinverse of matrix returned by makeCacheMatrix
## if previusly calculated, returns the matrix
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}