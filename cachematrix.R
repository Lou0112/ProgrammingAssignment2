## makeCacheMatrix extends the build-in matrix of R. It now can also
## store its own inverse. 
## cacheSolve determines the inverse of a makeCacheMatrix object x.

## This function instantiates a matrix object, which has additional
## information as its inverse and getters and setters

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) inv <<- solve
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function determines the inverse of the matrix x. First it 
## checks wether this value is already saved in x or not, and if so,
## the cached value is used. Otherwise, the inverse must be calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	# check if the inverse of x is already computed. If so, use it!
	if (!is.null(inv)) {
		message("getting cached inverse matrix")
		return(inv)
	}
	data <- x$get()
	# Inverse is not calculated yet, so do it now
	inv <- solve(data, ...)
	# Set the inverse of x
	x$setInverse(inv)
	inv
}
