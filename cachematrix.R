## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a mtrix and stores its inverse in cache

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    ## Set the matrix   
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    # Return the matrix
    get <- function() {x}
    
    # Calculate the inverse and store in cache
    setInverse <- function(inverse) {inv <<- inverse}
    
    # Return the inverse
    getInverse <- function() {inv}
    
    # Methods list
    list(set = set, get= get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
# This function get's the invers matrix from cache if it is already present
# If the matrix is not present in cache it takes new matrix calculates the
# inverse and stores it in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # Gettng the matrix from cache
    inv <- x$getInverse()
    
    # If matrix is present in cache return it directly
    if(!is.null(inv)){
        message('getting cached data')
        return(inv)
    }
    
    # If matrix is not present, calculate the inverse on the matrix
    # and store it in cache using the solve method
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}