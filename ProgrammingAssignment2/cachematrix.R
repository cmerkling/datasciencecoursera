## Assignment 2 

## This function sets the value of the matrix, 
## gets the value of the matrix, 
## sets the value of the matrix inverse, 
## and gets the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the matrix if it hasn't already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$getmatrix()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}