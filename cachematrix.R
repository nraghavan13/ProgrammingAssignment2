# Matrix inversion is computationally intensitve and optimized by using
# cache. Cachining the inverse of a matrix is more efficient 
# than an iterative process of performing matrix inversions. 

# makeCacheMatrix creates a special "matrix" object which performs
# the following functions:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inserve of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve computes the inverse of the special "matrix" returned by 
# makeCacheMatrix function. The function retrieves the inverse of the matrix
# from the cache if the inverse has already been calculated and does not 
# require any further computation. 
# Assumption: The matrix supplied is always invertible.
# Input: Special "matrix" output from makeCacheMatrix
# Output: Inverse of original "matrix" object

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
    if(!is.null(i)) {
    	message("getting cached data.")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    return(i)
}
