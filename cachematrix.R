## Put comments here that give an overall description of what your
## functions do

## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function that contains getter and setter functions to get a special
## matrix object and to set the value of the inverse matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
		    ##use <<- to save inversed matrix in cache
                x <<- y
                inv <<- NULL
        }
	  ##return matrix x
        get <- function() x
	  ##call function to save the inversed matrix in cache
        setInverse <- function(inverse) inv <<- inverse
	  ##return inverse of matrix
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function gets the cached inverse matrix if it exists and sets it
## using the makeCacheMatrix setter function if it doesn't

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
		    ##if inv is not null, it has been stored in the cache
                message("getting cached data")
		    ##return cached matrix
                return(inv)
        }
	  ##inverse does not exist, get matrix
        mat <- x$get()
	  ##create inverse of matrix
        inv <- solve(mat, ...)
	  ##call function to save inverse in cache
        x$setInverse(inv)
	  ##return inversed matrix
        inv
}
