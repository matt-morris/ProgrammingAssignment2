##############################################################################
##                                                                          ##
## Matrix inversion is usually a costly computation and there may be some   ##
## benefit to caching the inverse of a matrix rather than computing it      ##
## repeatedly (there are also alternatives to matrix inversion that we will ##
## not discuss here). Your assignment is to write a pair of functions that  ##
## cache the inverse of a matrix.                                           ##
##                                                                          ##
##############################################################################

##
# creates a special "vector", which is
# really a list containing a function to
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse matrix
# 4.  get the value of the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##
# Return a matrix that is the inverse of 'x'
# If the matrix has been previously solved,
# return the memoized (cached) result instead
# of re-calculating it.
##
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
