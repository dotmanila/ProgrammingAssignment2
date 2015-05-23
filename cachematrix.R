# Programming Assignment 2
#  Caching solve()d inverse of a matrix using <<- operator
#
# Complete Example: 
# m <- matrix(1:4, 2, 2)
# c <- makeCacheMatrix(m)
# debug(cacheSolve)
# cacheSolve(c)
# debug(cacheSolve)
# cacheSolve(c)

# This function creates a special "matrix" object 
# that can cache its inverse using the "solve" function
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
    setsolve = setsolve,
    getsolve = getsolve)
}

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'

    # We check if its in our special matrix cache first
    m <- x$getsolve()

    if(!is.null(m)) {
        # We have a cache hit
        message("getting cached data")
        return(m)
    }

    # If its a cache miss, we solve and store
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
