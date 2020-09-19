## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function takes a matrix "x" and creates a special "matrix" (not an actual matrix but a list)
## composed of functions that can get/set the actual matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
    
        #Sets "x" to the value of "y", from another environment.
        #Sets the inverse to null because the matrix changes.
        setX <- function(y) {
            x <<- y
            inv <<- NULL
        }
    
        #Returns x.
        getX <- function() x
    
        #Sets the value of "inv" to the value of "inver", also from another environment.
        setInv <- function(inver) inv <<- inver
    
        #Returns inv.
        getInv <- function() inv
    
        #Returns the "special matrix", aka the list of functions that get and set the value of the
        #actual matrix as well as its inverse.
        list(setX = setX, getX = getX, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

## Calculates the inverse of the matrix based on the "special matrix" created
## in makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        inv <- x$getInv()
        
        #Check if the inverse has already been computed. If so, returns it from the cache.
        if(!is.null(inv)) {
            message("Retrieving the cached inverse of x.")
            return (inv)
        }
        
        #If the inverse is not cached, we retrieve the matrix and compute its inverse.
        currentX <- x$getX()
        inv <- solve(currentX)
        
        #We set the inverse inside the "special matrix".
        x$setInv(inv)
        
        #Finally we return the inverse.
        inv
}
