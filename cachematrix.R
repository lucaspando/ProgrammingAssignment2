##These functions allow you to cache the potentially time-consuming 
##computations that requires to calculate the inverse of a squared matrix.

## This function creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  
  cachedInverse <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  getMatrix <- function() x
  
  setInverse <- function(solve) cachedInverse <<- solve
  
  getInverse <- function() cachedInverse
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)  
  
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
  
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  
  data <- x$getMatrix()
  
  inverse <- solve(data)
  
  x$setInverse(inverse)
  
  inverse  
}