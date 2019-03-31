## In this assignment we are making a set of functions which will
## help us make the the inverse of a matrix. Inversing a matrix is
## usually a tedious computation hence caching.

## In this function special matrix i created that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inversed <<- inverse
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If already inversed it will retrieve from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}