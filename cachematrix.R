## This package of functions functionally supersets the matrix 
## so that we can associate a cached version of it's inverse.
## It consists of two functions...
##    makeCacheMatrix
##    cacheMatrix

## The function makeCacheMatrix creates a matrix that also may hold
## a cache of it's inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            ## first set x in the parents enviornment
            x <<- y
            inv <<- NULL
            ## we haven't yet computed the inverse so...
      }
      ## just returns the original matrix
      get <- function() x
      
      ## this bit doesn't compute the inv, just sets it
      setInv <- function(mInv) inv <<- mInv
      
      ## this bit is where we return the cached value
      getInv <- function() inv
      
      ## i'm a little shaky on why we do this bit. for return?
      list (set=set, get=get, setInv=setInv, getInv=getInv)
}


## the function cache solve caches the result of running solve on
## a matrix and associates that cached version with said matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInv()
      ##checks to see if it's already done
      if(!is.null(inv)) {
            message("retrieving cached inverse")
            return(inv)
      }
      ## if not done, let's do it
      inv <- solve(x$get(),...)
      ## caching for later use
      x$setInv(inv)
      ## and returning the inverse as expected
      inv
}
