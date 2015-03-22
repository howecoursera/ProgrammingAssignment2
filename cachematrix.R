#############################################################################
## Put comments here that give an overall description of what your
## functions do
## author: Hao Zhi
## date:   03/22/2015
## for R programming coding assignment 2 - Caching the Inverse of a Matrix.
#############################################################################


## Write a short comment describing this function
## FUNCTION makeCacheMatrix():
##  - maintain last calculated inverse matrix in cache if matrix remains the
##    same; otherwise cache is reset. 
##  - also provide read/write api for get/set target matrix and cache.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL           ## cache.
    set <- function(y){ ## set function that changes matrix.
        if (identical(x,y)==F) { ## iff matrix changes, should we reset cache.
            x <<- y
            i <<- NULL
        }
    }
    get <- function() x            ## get function that reads matrix.
    setinverse <- function(solve){ ## set function that changes cached result. 
        i <<- solve
    }
    getinverse <- function() i     ## get function to query cache.
    list(set = set,                ## list of api.
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## FUNCTION cacheSolve():
##  - return cached inverse matrix iff cache has not been evicted; 
##    otherwise recalculate inverse matrix and cache it in for potential future 
##    usage.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){ ## iff already cached, just query it for performance. 
        message("getting cached inverse matrix.")
        return(i)
    }
    ## Otherwise recompute inverse matrix and set it to cached result.
    data <- x$get()
    i <- solve(data, ...)
    message("updating new inverse matrix into cache...")
    x$setinverse(i)
    i
}
