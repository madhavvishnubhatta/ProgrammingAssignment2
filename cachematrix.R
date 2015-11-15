## There are two functions in this file. The first function creates
## a special object (a list) that provides the functionality of a cached Matrix.
## The second function calculates the inverse of a special cached matrix object.

## This function defines the special cached matrix object
## It returns the object (which is a list) that contains the following functions
## get - Gets the matrix
## set - Sets the matrix
## getinverse - Gets the Inverse of the matrix
## setinverse - Calculates the inverse of the matrix

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
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function calculates the inverse of the special matrix object
## If the inverse is already calculated it uses the existing cached inverse (after printing a message to that effect)
## If not, it calculates it and sets it

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data,...)
	x$setinverse(i)
	i
}

