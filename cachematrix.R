## To avoid unnecessary repeated calculation of a matrix' inverse when it hasn't
## changed, makeCacheMatrix wraps a matrix and returns a list of accessor 
## functions in order to keep track of whether or not a recalculation has become
## necessary since it's last been solved.
##
## To take advantage of this chaching, the cacheSolve function must be used to 
## solve the matrix.
##


## makeCacheMatrix takes a matrix as its argument and returns a list of
## functions that can be used to access, solve, or replace the matrix with a new
## one. If no matrix is passed as an argument, a new one is created.

makeCacheMatrix <- function(x = matrix()) {
    # inverse contains the last calculated inverse, or NULL if
    # the matrix has changed since the last solve.
    inverse <- NULL
    
    # Retrieve or set the matrix.
    get <- function () x
    set <- function (value) {
        x <<- value
        inverse <<- NULL
    }
    
    # Retrieve the last inverse, or NULL if it has to be recalculated.
    getSolve <- function ()         inverse
    
    # Set the inverse after a (re-)calculation for later retrieval.
    setSolve <- function (value)    inverse <<- value
    
    # Create and return a list object with accessor functions for the matrix.
    list(set = set,
         get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## The cacheSolve function takes the list object created by makeCacheMatrix
## above as an argument. It solves the matrix if necessary, or recycles a 
## previously calculated inverse when necessary.

cacheSolve <- function(x, ...) {
    # Retrieve the previously calculated inverse, or NULL if the matrix has
    # changed since the last solve.
    inverse <- x$getSolve()
    
    # Check if a solved inverse has already been cached.
    if (!is.null(inverse)) {
        # The matrix hasn't changed and we can recycle the inverse.
        return(inverse)
    }
    
    # The matrix has changed or is new, calculate the inverse and cache it for
    # later reuse.
    inverse <- solve(x$get(), ...)
    x$setSolve(inverse)
    
    # Return the inverse.
    inverse
}