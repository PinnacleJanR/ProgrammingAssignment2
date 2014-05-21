makeCacheMatrix <- function(x) {        ##prepares a matrix inversion to be cached
        m <- NULL                       ##creates m as yet empty
        set <- function(y) {            ##creates the set function that sets the matrix values
                x <<- y
                m <<- NULL
        }
        get <- function() x             ##creates the get funtion that returns the matrix values
        savecache <- function(inversematrix) m <<- inversematrix  ##creates the savecache function that stores the values of inverse matrix in cache
        getcache <- function() m        ##creates the getcache function gets the values that are stored in cache
        list(set = set, get = get,      ##makes a list of the 4 created functions
             savecache = savecache,
             getcache = getcache)
}

cachesolve <- function(x, ...) {        ##returns the inverse of a matrix by either calculating it or getting value from cache
        m <- x$getcache()               ##gets the value from cache
        if(!is.null(m)) {               ##checks if the value is NULL
                message("getting cached data")
                return(m)               ##if value was cached before this value is returned
        }
        data <- x$get()
        m <- solve(data, ...)           ##if no value was in cache the inverse matrix is calculated
        x$savecache(m)                  ##result is stored in cache
        m                               ##result is printed
}