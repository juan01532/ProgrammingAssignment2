

## this function provides a list of functions that will be used by the cacheSolve
## function.

makeCacheMatrix <- function(x = matrix()) {
    ## first we set our variable to NULL
    inv <- NULL
    ## the set function substitutes the matrix X with the matrix Y, and sets the value
    ## of inv to null
    set <- function(y){
        x <<- y
        inv <<- NULL
    } 
    
    ## the function get retrieves the value of X
    get <- function() x
    
    ## setinv sets the variable inv to the value of inverse
    setinv <- function(inverse) inv <<- inverse
    
    ## getinv gets the value of inv
    getinv <- function() inv
    ## this las line creates a list of the above functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## the cacheSolve function returns the inverse of a matrix, 
## it first checks to see if there is a cached version, and if not
##calculates de result and returns it.

cacheSolve <- function(x, ...) {
    ## first it gets the stored value of inv, and checks if it is null, if not
    ## it returns a value
    inv <- x$getinv()
    if(!is.null(inv)){
        message("Retrieving cached data")
        return(inv)
    }
    ## if inv is null, it calculates the inverse of the matrix.
    data <- x$get()
    inv <- solve(data)  
    x$setinv(inv)
    inv
    ## this returns a matrix that is the inverse of 'x'
}






