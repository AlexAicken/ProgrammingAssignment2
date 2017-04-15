## High level - Creates a method to calculate the inverse of an invertible matrix
## If the inverse has already been calculated, it uses this cached inverse (saving calculation time)

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse - 
## A list containing as function to: get the matrix, set the matrix, get the inverse, set the inverse

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
      x <<- y
      matrixInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) matrixInverse <<- inverse
    getInverse <- function() matrixInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  matrixInverse <- x$getInverse()
  
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  
  message("not using cached data")
  
  data <- x$get()
  matrixInverse <- solve(data, ...)
  x$setInverse(matrixInverse)
  return(matrixInverse)
}
