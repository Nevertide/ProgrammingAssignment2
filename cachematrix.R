#1 makeCacheMatrix: creates an object containing the following lists:
      # set - Sets the value of the matrix.
      # get - retrives the set value of the matrix.
      # setinverse - sets the values solved inverse matrix
            # Does not compute, only stores the value.
      # getinverse - retrives the set inverse value of the matrix.
#2 cacheSolve: Either retrives the cached solution of the
#              inverse matrix or solves, stores, and returns it.


## Creates a special matrix that contains functions
## capable of caching and retrieving its inverted version.

makeCacheMatrix <- function(x = matrix()) {
      ## Initailize the null inverse variable.
      inv_m <- NULL
      ## Function set values.
            # y as argument, reset inverse variable when called.
      set <- function(y) {
            x <<- y
            inv_m <<- NULL
      }
      ## Return variable defined by set function.
      get <- function() x
      ## Set the inverse matrix values (does not compute, only stores).
      setinverted <- function(solve) inv_m <<- solve
      # Retrieves the value of the inverted matrix.
      getinverted <- function() inv_m
      # Returns a list of the above functions (psuedo-object style).
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
      ## Sets the inverted value to a cached value if it exists...
      inv_m <- x$getinverted
      ## checks for existence, if so - returns value.
      if(!is.null(inv_m)) {
            message("getting cached data")
            return(inv_m)
      }
      ## If not, retrives the matrix value from the object.
      data <- x$get
      ## Calculates the inverse matrix and assigns to inv_m variable.
      inv_m <- solve(data, ...)
      ## Caches the solved matrix in the makecacheMatrix "object".
      x$setinverted(inv_m)
      ## Finally, returns the value.
      inv_m
}
