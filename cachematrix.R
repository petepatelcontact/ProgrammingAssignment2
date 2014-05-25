## The functions below provide calculation of the inverse of a Matrix,
## a costly computation.  To optimize repeated calls on the same matrix a 
## caching function has been implemented to return a previously calculated
## inverse of a matrix rather than do the computation again on the same matrix.
##
## There are two functions to address this optimization:
##  makeCacheMatrix - caches inverse of the matrix
##  cacheSolve - computes the inverse of the matrix
##

## Create a matrix object that can cache its inverse
##    Initialize a variable m to NULL and set it to the inverse on 
##    call to the set function and return the inverse
##
##    On the getinverse call return the cached m matrix  
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
	        x <<- y
		m <<- NULL
        }
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
	        setinverse = setinverse,
		getinverse = getinverse)

}


## Call the getinverse to get the cached m inverse matrix
## If the cached value is NULL then call the setinverse function to
## calculate the inverse.
## If the casched value is returned print a message indicating that
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if (!is.null(m)) {
	        message("getting cached data")
		return(1)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
