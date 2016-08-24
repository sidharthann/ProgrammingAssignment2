## Functions to calculate the inverse of a matrix.
## The inverse of a matrix is cached when it is calculated for
## the first time, so that the calculation does'nt have to be repeated
## when we ask for the inverse of the same matrix again.

## This function creates a matrix cache that stores the inverse of the matrix 
## along with functions for retrieving the matrix and its cached inverse. 
## The cached inverse is initialised to NULL when a new matrix is created or 
## the underlying matrix is modified.
makeCacheMatrix <- function(mat = matrix()) {
    inverse <- NULL
    set <- function(y){
        mat <<- y
        inverse <<- NULL
    }
    get <- function() mat
    setinverse <- function(inv){
        inverse <<- inv
    }
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}


## This function returnes the inverse of a matrix.
## If the inverse exists in the cache, no additional calculations are
## performed and the cached value is returned. If not, the inverse is calculated using solve()
## and the result is also cached. Additional arguments supported by the solve function can be passed to this function
## and will have the same effect as in solve().
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        print("Getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
    
}
