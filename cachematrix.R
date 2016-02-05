## A pair of functions that cache the inverse of a matrix

## This function creates an object that contains matrix and its inverse. 
## It returns a list of setters and getters for these "properties". 
## Lexical scoping allows reaching them from set and get functions, 
## where x and i are free variables. 
## Values for getters and objects that should be assigned with a new value 
## for setters are found in the environment, in which these
## functions were defined - that is the body of the makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## This function looks for the inverse in the cache of the "Matrix" object.
## If the inverse matrix is not found, it is computed.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
