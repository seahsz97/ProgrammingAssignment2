## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix and cacheSolve solves and cache the inverse of a matrix. This  
# makes the computation for matrix inversion less costly and speeds up the 
# process as the function can retrieve the already solved matrix.

## Write a short comment describing this function
# makeCacheMatrix builds a set of functions and returns the functions within
# a list to the parent environment, allowing users to "set" and "get" the 
# matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverseMatrix <<- inverse
        getInverse <- function() inverseMatrix
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve completes the makeCacheMatrix function. cacheSolve is 
# the function that helps to update and cache the inverse matrix in 
# makeCacheMatrix. It is the function that obtains the inverse of the matrix.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}
