## cachematrix.R
##
## Description
##
## This file provides an object for holding a matrix along with the
## cached value of its inverse.  (These two items are stored together
## within a function closure.)
##
## Usage
##
##     cm <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
##     cm$get()
##     cacheSolve(cm)
##     cm$getinverse()
##

## makeCacheMatrix
##
## Description
##
## Creates a matrix object which caches its inverse upon a call to
## cacheSolve.
##
## Usage
##
##     cm <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
##     cm$get()
##     cm$set(m)
##     cm$setinverse(i)
##     cm$getinverse()
##
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # the cached inverse of x
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve
##
## Description
##
## Solves for the inverse of a cached matrix.  If the inverse is
## already cached, it is returned.  Otherwise the inverse is computed,
## cached, and returned.
##
## Usage
##
##     cacheSolve(cm)
##     cm$getinverse()
##
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    i <- solve(x$get(), ...)
    x$setinverse(i)
    i
}
