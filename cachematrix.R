## This module implements a cache for computing inverse matrix
## The usage is like this:
## m1 <- matrix(1:4, 2, 2)
## cacheSolve(makeCacheMatrix(m1))

## this function implements the basic features of cache matrix get/set
makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) v <<- inverse
        getinverse <- function() v
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function compute inverse matrix using cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        v <- x$getinverse()
        if(!is.null(v)) {
                message("getting cached inverse")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setinverse(v)
        v
}