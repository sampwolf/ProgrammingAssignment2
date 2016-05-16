## Coursera Assignment # 2
## 15 de Maio 2016
## Cache values - Matrix - Lexical Scoping
#
# I`ll make pair of functions that cache the inverse of a matrix.
# # # # # #

## # # # # #
# This function creates a special "matrix" object that can cache its inverse.
#
# If X is a square invertible matrix, then solve(X) returns its inverse
# # # # # # #

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <- function(y){
                x<<-y
                m<<-NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list( set = set, get = get, 
              setinverse = setinverse,
              getinverse = getinverse)
}


# # # # # # #
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above.
#
# If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
# # # # # # #

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("gettiing cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
