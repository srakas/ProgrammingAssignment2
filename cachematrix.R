## This script contains functions which are used to calculate 
## and cache the inverse of the matrix

## Function creates matrix cache from the matrix 'x'
makeCacheMatrix <- function(x = matrix()) {
    inverseMat <- NULL
    set <- function(y) {
        x <<- y
        inverseMat <<- NULL
    }
    
    get <- function() x
    setInverseMat <- function(i) {
        inverseMat <<- i    
    }
    getInverseMat <- function() inverseMat
    list(set = set, get = get, setInverseMat = setInverseMat, getInverseMat = getInverseMat)
}

## Function returns a matrix that is the inverse of 'x'
## If the matrix did not change and inverse has already been calculated 
## the inverse is pulled from the cache, otherwise calculates matrix inverse

cacheSolve <- function(x, ...) {
    inverseMat <- x$getInverseMat()
    if (!is.null(inverseMat)) {
        message("Getting cached data")
        return(inverseMat)
    }
    data = x$get()
    i <- solve(data)
    inverseMat <- x$setInverseMat(i)
    inverseMat    
}
