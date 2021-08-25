## This function calculates the inverse matrix if it is invertible 

## This function cache the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() {inv}
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## This function calculates the inverse of a matrix

cacheinverse <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting Cached Data")
        return(inv)
    }
    matrix_to_invert <- x$get()
    inv <- solve(matrix_to_invert, ...)
    x$setinverse(inv)
    inv
}

## Returns the inverse matrix