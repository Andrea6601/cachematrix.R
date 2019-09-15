The following two functions aim to cache the inverse of a matrix.
The first function makeCacheMatrix, creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <- NULL 
                }
                get <- function() x 
                        setInverse <- function(solveMatrix) a <<- solveMatrix
                        getInverse <- function() a
                        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

The second function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        a <- x$getInverse()
        if(!is.null(a)){
                message("getting cached data")
                return(a)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(a)
        a
}
