## Put comments here that give an overall description of what your
## functions do

## Functions to support caching the inverse of a matrix so that the inverse
## value does not need to be calculated if the matrix has not changed

## Write a short comment describing this function

## creates a matrix for which the inverse can be cached
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## returns the inverse for the matrix
## 1) from the cache if it has been set for the matrix
## 2) otherwise it will calculate the inverse and save it to the cache
##    before returning the calculated inverse value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
