## Caching the inverse of a Matrix
## The functions first creates a special "matrix" object which can then be caches for its inverse.

## The functions first creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y){
				x <<- y
				i <<- NULL
	}
		get <- function() x
		setinverse <- function(inverse) i <<- inverse
		getinverse <- function()i
		list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
	
}


## This function computes the inverse of the special "matrix"returned
## by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
        		message("getting cached data")
        		return(i)
        }
        dat <- x$get()
        i <- solve(dat, ...)
        x$setinverse(i)
        i
}
