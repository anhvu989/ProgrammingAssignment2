## This pair of functions will cache the inverse of a matrix

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(matrix) {
        x <<- matrix
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: computes the inverse of the "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    inv
}


