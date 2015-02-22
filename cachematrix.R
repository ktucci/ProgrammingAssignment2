## With the functions of this script can cache the inverse 
## of a matrix instead of compute it repeatedly

## Creates a special "matrix", which is really a list containing 
## a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(A = matrix()) {
    I <- NULL
    set <- function(y) {
        A <<- y
        I <<- NULL
    }
    get <- function() A
    setInverse <- function(solve) I <<- solve
    getInverse <- function() I
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This fuction calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks 
## to see if the inverse has already been calculated. If so, 
## it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets 
## the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(A, ...) {
    I <- A$getInverse()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- A$get()
    I <- solve(data, ...)
    A$setInverse(I)
    I   
}
