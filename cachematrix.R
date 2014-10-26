## This pair of functions work together to permit cacheing of matrix inversions.
## 'makeCacheMatrix' sets up the cache data structure, and 'cacheSolve' replaces
## the 'solve' function in R, returning the cached solution instread, if appropriate.

## the function 'makeCacheMatrix' creates a special "matrix" object that can cache 
## its inverse. This object is a list containing four functions:
## (1): set the value of the matrix
## (2): get the value of the matrix
## (3): set the value of the inverse
## (4): set the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function () inv
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## the function 'cacheSolve' computes the inverse of the special 'matrix' created
## with the above function. It first checks to see if the inverse has already been
## computed. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it computes the inverse of the matrix and sets this value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    # message('getting cached data')
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
