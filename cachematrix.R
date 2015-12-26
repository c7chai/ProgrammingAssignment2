## A pair of functions that cache the inverse of a matrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the cache
     m <- NULL
  
  ## Set the matrix
     set <- function( matrix ) {
       x <<- matrix
       m <<- NULL
     }
     
  ## Get the matrix
     get <- function() {
  
  ##Return the matrix
       x
     }
     
  ## Set the inverse of the matrix
     setInverse <- function(inverse) {
       m <<- inverse
     }
     
  ## Get the inverse of the matrix
     getInverse <- function() {
  
  ## Return the inverse matrix
       m
     }

  ## Return a list of the methods
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix()
## If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
     m <- x$getInverse()
     
  ## Check to see if the inverse matrix has already been calculated. 
  ## If so, it gets the inverse from the cache and skips the computation.
     if( !is.null(m) ) {
       message("getting cached data")
       return(m)
     }
     
  ## Get the matrix 
     data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
     m <- solve(data) %*% data
       
  ## Set the inverse 
     x$setInverse(m)
      
  ## Return the matrix
    m
     
}
