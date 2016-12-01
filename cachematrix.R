
# Caching is done to save time and cost on repetitive operations. 
# The following two functions are used to cache the inverse of a matrix.
# For this assignment, the matrix supplied is always invertible.


# makeCacheMatrix creates a list that
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse= setinverse, 
	   getinverse= getinverse)
}



# The following function calculates the inverse of the matrix. However,it first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. Otherwise, it computes the inverse, sets the value in the cache via the
# setinverse function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data.")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
