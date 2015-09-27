##
## Create a cache marix object that can be used to
## repeatably solve the inverse of the marix, but only
## calculates the inverse once.

## Create a cacheMatrix object for an invertale matrix.

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) cachedInverse <<- inverse
  
  getInverse <- function() cachedInverse
  
  # Return a list with the above four functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getInverse()
  
  if(!is.null(invFunc)) { # If the cache was not empty, we can just return it
    message("getting cached data")
    return(invFunc)
  }
  
  data <- x$get() # Get value of matrix
  
  invFunc <- solve(data) # Calculate inverse
  
  x$setInverse(invFunc) # Cache the result
  
  invFunc # Return the inverse
}

## Sample run:
##  M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheMatrix$get()
##        [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4
##  cacheSolve(cacheMatrix)
## getting cached data
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
## Retrieving from the cache in the second run
##  cacheSolve(cacheMatrix)
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
