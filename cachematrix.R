## Below are a pair of functions that cache the inverse of a matrix.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	get <- function() x
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	return(list(set=set,
				get=get,
				setinverse=setinverse,
				getinverse=getinverse))
}

## Returns a matrix that is the inverse of 'x'.
cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()
	if (!is.null(inverse)) {
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data)
	x$setinverse(inverse)
	return(inverse)
}
