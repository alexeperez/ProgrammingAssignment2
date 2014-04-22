## The following pair of functions show advantages of lexical
## scoping in R. We study the benefit to caching the inverse 
## of a matrix (costly computation)


## The following function creates a special object that stores
## a matrix and cache's its inverse. It creates a list of some
## functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # Set a matrix if x has been changed
        setmatrix <- function(y) {
                x <- y
                inv <<- NULL
        }
        # get matrix x 
        getmatrix <- function() x
        # cache object a
        setinv <- function(a) inv <<- a
        # get inv from cache
        getinv <- function() inv
        list(setmatrix = setmatrix,
             getmatrix = getmatrix,
             setinv = setinv,
             getinv = getinv)
}

## The following function computes the inverse matrix of x
## (created with the function makeCacheMatrix).It first 
## checks if inverse has been calculated, if so, it gets
## inverse from cache

cacheSolve <- function(x, ...) {
        ## Get inverse matrix from cache
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Compute and set inverse matrix 
        data <- x$getmatrix()
        inv <- solve(data, ...)
        x$setinv(inv)
        return(inv)
}

