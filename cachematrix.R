## Carlo Ordonez
## R Programming - Programming Assignment 2
## This file has two functions. The first takes in a matrix as an argument
## and creates a matrix with a cache for its inverse. The second function
## will find the matrix inverse either by reading the cache or computing
## it and then storing it in its cache.
## These functions were modified from the vector examples given in the 
## Programming Assignment 2 instruction page.

## This function creates a matrix with a cache for its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
}


## Given a cached matrix, this function returns the inverse
## of the matrix, either by calling it from the cache, or 
## computing the inverse and then storing it in the cache
## for later.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
