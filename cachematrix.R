## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = numeric()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) (I <<- Inverse)
  getInverse <- function() (I)
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cachesolve <- function(x, ...) {
  I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  mat <- x$get()
  I <- solve(mat, ...)
  x$setInverse(I)
  I
}
