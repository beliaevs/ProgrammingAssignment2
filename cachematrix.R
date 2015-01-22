## This file contains implementation of cache-enabled "matrix" object, that caches once calculated inverse matrix
## "Matrix" is a list of functions that can:
##  set the value of matrix, inverse matrix is invalidated
##  get the value of matrix
##  set the value of inverse matrix
##  get the value of inverse matrix (NULL if not calculated yet)

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        #cached inverse matrix
        inv <- NULL
        
        #change initial matrix
        set <- function(y) {
            x <<- y
            inv <<- NULL        
        }
        
        #get initial matrix
        get <- function() x
        
        #get cached inverse matrix
        getinv <- function() inv
        
        #cache inverse matrix
        setinv <- function(i) inv <<- i 
        list(set = set, get = get, setinv = setinv, getinv = getinv)
 }


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## ... parameters are passed to solve() function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i))
        {
            #message("get cached inversed matrix")
            return(i)
        }
        #compute inverse if not yet calculated
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
