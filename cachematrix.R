## Catching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than
## computing it repeatedly.

## makeCacheMatrix : This function creates a special "matrix" object
## that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set,
		 get = get,
		 setInverse = setInverse
		 getInverse = getInverse)
}


## cacheSolve : This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed,) then
## cacheSolve should retreive the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
        		message("getting cached inverse matrix")
        		return(inv)
        }	
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv	
}
