## These functions create a cache-able matrix, and invert and cache it

## Ccreate a cache-able matrix
## Provides functions set() and get() to store and retrieve the
## underlyiing matrix. setInv() and getInv() functions save and retrieve the
## inverse of the underlying matrix.
makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    set <- function(y) {
        x <<- y
        mInv <<- NULL
    }
    get <- function() x
    setInv <- function(s) mInv <<- s
    getInv <- function() mInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve() inverts a given cache-able
## matrix, caches the result and returns the inverted matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mInv <- x$getInv()
    if(!is.null(mInv)) {
        message("getting cached data")
        return(mInv)
    }
    data <- x$get()
    mInv <- solve(data, ...)
    x$setInv(mInv)
    mInv
}

