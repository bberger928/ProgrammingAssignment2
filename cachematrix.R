## Author: Brendan Berger

## These functions allow the inversing of matrices to be cached

## This function converts a matrix to a data type that enables cacheing.

makeCacheMatrix <- function(data = matrix()) {
    cachedVal <- NULL
    set <- function(newdata) {
        data <<- newdata
        cachedVal <<- NULL
    }
    get <- function() data
    setinverse <- function(inv) cachedVal <<- inv
    getinverse <- function() cachedVal
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This is used instead of solve to communicate with the cache and
## potentially its value instead of re-evaluating the inverse.
## This function assumes that x is inversable

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("returning cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
