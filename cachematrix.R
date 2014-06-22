## These functions are to solve problem of performance when there is need to
## calculate inversed matrix. Basically implemented functionality increases
## performance only in case when we need to calculate more than one time inversed
## matrix for same input matrix. Such solution is called caching. Once inversed matrix
## is calculated it will be returned already prepared.

## makeCacheMatrix function gets matrix x as input and creates special "vector"
## which containes setters and getters for input matrix and resulted inversed matrix
## So when such "vector" is being created it stores only input x value but not inversed matrix
## Once inversed matrix will be calculated it is stored intho this "vector" by additional handler
## function called cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        cached_inverse <- NULL
        set <- function(y) {
                x <<- y
                cached_inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inversed) cached_inverse <<- inversed
        getinv <- function() cached_inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve function gets special "vector" x as input param
## and checks if inversed matrix is calculated. If so - it returns inversed matrix.
## Otherwise it calculates inversed matrix, stores it into x and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inversed matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
