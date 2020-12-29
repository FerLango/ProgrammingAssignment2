## Functions that cache the inverse of a matrix


## Creates a matrix that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
    i <- NULL
    set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
    }
    get <- function() {
        m
    }
    setInverse <- function(inverse) {
        i <<- inverse
    }
    getInverse <- function() {
        i
    }
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the matrix returned by "makeCacheMatrix"
## "cachesolve" retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}