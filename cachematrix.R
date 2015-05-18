
makeCacheMatrix <- function(x = matrix()) {
    # Faciliates the caching of a matrix inversion calculation
    # The created object stores the provided matrix and the calculated inverse matrix.
    # This function is intnded to be used in conjunction with cacheSolve
    #
    # Args: 
    #   x: The matrix whose inverse is to be cached
    #
    # Returns:
    #   A list containing assessor functions for the matrix and it's inverse calculation
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get=get, setinverse = setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
    # Computes the inverse of the provided matrix. If the inverse matrix has 
    # been calculated previously, a cached results will be returned to avoid
    # recalculation.
    #
    # Args: 
    #   x: A special matrix object created with makeCacheMatrix
    #
    # Returns:
    #   A matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached matrix inverse")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m)
    x$setinverse(inv)
    inv
}
