## Since solving matricies can be a time consuming operation, these functions
## will set up a system of caching the inverse of the current matrix and only
## calculate the inverse if the inverse has not yet been calculated for the current
## version of the matrix.

## This function creates a special "matrix" object that can cache its inverse.
## The object has 4 functions in it that set the matrix, get the matrix,
## set the inverse of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	invmtx <- NULL
	set <- function(y) {
		x <<- y
		invmtx <<- NULL
	}
	get <- function() x
	setinv <- function(inv) invmtx <<- inv
	getinv <- function() invmtx
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache. If the inverse has not been calculated, then
## calculate it, save the inverse and return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        inv <- solve(x$get(), ...)
        x$setinv(inv)
        inv
}
