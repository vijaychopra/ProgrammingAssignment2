## makeCacheMatrix and cacheSolve functions are used together to find inverse of a matrix.
## makeCacheMatrix caches the value of the inverse of the matrix so that it can be looked up 
## in the cache when we need it again instead of being recomputed.

makeCacheMatrix <- function(x = matrix()) {
  ## inverse is Cache variable which stores the value of the inverse
  inverse <- NULL 
  ## setmatrixfunction sets the cache variable with the matrix whose inverse has been already calculated
  setmatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## getmatrix function gets the matrix whose inverse needs to be calculated
  getmatrix <- function() x
  ## setinverse function sets the cache variable with the inverse value for the matrix
  setinverse <- function(inversematrix) inverse <<- inversematrix
  ## getinverse function returns the inverse value present in cache
  getinverse <- function() inverse
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve checks if the inverse of the matrix was already computed and is present in the 
## cache before calculating it. If its already present, it uses the value from the cache. If 
## not, it recalculates it.

cacheSolve <- function(x, ...) {
  ## Here is is being checked if the inverse had been already calculated
  ## If the inverse value is already present in cache, then the same is returned.
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$getmatrix()
  ## If the inverse value is not present in cache, then it is calculated.
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
