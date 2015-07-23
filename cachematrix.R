## title: cachematrix.R
## author: Andrew Martinez
## date: July 20, 2015
## output: console

## This assignment is to write a pair of functions that cache the inverse of a matrix.

## `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
                i <- NULL # initialize inverse internal object
                set <- function(y) { # set internal matrix
                    x <<- y
                    i <<- NULL
                }
                get <- function() x # get internal matrix
                setinverse <- function(inv) i <<- inv # set cache object
                getinverse <- function() i # get cache object
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse) # setup internal properties
}

## `cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
                i <- x$getinverse() # try to load inverse from cache
                if(!is.null(i)) {   # check if object loaded from cache
                    message("getting cached data") 
                    return(i) # return a matrix that is the inverse of 'x' that was loaded from cache
                }
                data <- x$get() # get internal matrix object
                i <- solve(data, ...) # inverse matrix
                x$setinverse(i) # set inverse result to cache
                i # return a matrix that is the inverse of 'x' that was calculated
}
