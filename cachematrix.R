## Put comments here that give an overall description of what your
## functions do

## Creates a "special matrix object" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    data <- x
    inverse <- NULL
    getData <- function() data
    setData <- function(newData) {
        data <<- newData
        inverse <<- NULL
    }
    getInverse <- function() inverse
    setInverse <- function(i) inverse <<- i
    list(getData = getData, setData = setData, getInverse = getInverse,
         setInverse = setInverse)
}


## Computes the inverse of a "special matrix", or retrieves it from the cache
## if it has already been calculated.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (is.null(inverse)) {
        data <- x$getData()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
    } else {
        message('getting cached data')
    }
    return(inverse)
}
