## Put comments here that give an overall description of what your
## functions do

## Generate function vector for cacheing a matrix and its inverse.
## The returned function vector contains the following methods:
##   - setMatrix() must be called with a square numeric or complex matrix
##   - getMatrix() returns the last matrix last provided to makeCacheMatrix() 
##     or setMatrix(),
##   - setInverse() sets the inverse matrix, and must be called with a 
##     square numeric or complex matrix
##   - getInverse() returns the last matrix provided to setInverse(),
##     or NULL if setInverse() has not been called

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculate and cache the solution to the matrix stored in x. Functionally 
## equivalent to solve(x$getMatrix(), ...), but cacheSolve() is more efficient
## when it is called multiple times with the same matrix, due to cacheing
## the solution.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$getMatrix()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
