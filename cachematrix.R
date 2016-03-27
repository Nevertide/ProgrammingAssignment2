## Creates a special "matrix" that contains functions allowing
## the storage and retrivel of its inverted matrix.

## Creates a special matrix that contains functions
## capable of caching and retrieving its inverted version.

makeCacheMatrix <- function(x = matrix()) {
      inv_m <- NULL
      set <- function(y) {
            x <<- y
            inv_m <<- NULL
      }
      get <- function() x
      setinverted <- function(solve) inv_m <<- solve
      getinverted <- function() inv_m
      list(
            set = set
            ,get = get
            ,setinverted = setinverted
            ,getinverted = getinverted
      )
}


## Function that, if possible, retrives the cached inverted matrix of
## its argument, or otherwise computes and stores the value for future
## retrieval.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv_m <- x$getinverted
      if(!is.null(inv_m)) {
            message("getting cached data")
            return(inv_m)
      }
      data <- x$get
      inv_m <- solve(data, ...)
      x$setinverted(inv_m)
      inv_m
}
