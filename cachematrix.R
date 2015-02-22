## The purpose of these functions is to cache the inverse of a matrix. 

## This function creates a matrix object. It is used in conjunction with the
## cacheSolve() function below to compute the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse,
		getinverse = getinverse)
}


## This function cacheSolve, computes the inverse of the matrix created above.
## If the matrix inverse has already been calculated, it is returned from the
## cache (value stored in i) instead of recalculating it. 

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
