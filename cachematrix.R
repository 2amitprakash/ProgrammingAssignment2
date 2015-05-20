## Description of implementation for the Functions:
############
## makeCacheMatrix(x) function will build a wrapper around the matrix in parameter and
## create a special object. This object is actually implemented as list of following 
## functions:
##    set the value of the matrix (wrap the matrix)
##    get the value of the matrix (returned the contained matrix)
##    set the value of the inverse
##    get the value of the inverse
############
## cacheSolve(x, ...) function will accept an object returned by makeCacheMatrix(x) and
## check if there is already a cached value. If the cached value is present then simply
## return the value else use the solve(a) function to get the inverted matrix.
############

## This function creates a special "matrix" object (which is really a list of functions) 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## initialize the local variable.
    
    ## define the set (y) function.
    set <- function(y) {
        x <<- y  ##To change the value of x in enclosing environment i.e. the formal parameter 
        m <<- NULL ##To change the value of m in enclosing environment i.e. the above local variable 
    }
    get <- function() x  ## access the variable set by set(y).
    
    ##To change the value of m in enclosing environment i.e. the above local variable
    setinv <- function(inv) m <<- inv 
    
    getinv <- function() m ## access the variable set by setinv(inv).
    
    ## return the special list.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getinv() 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get() ## get the wrapped actual matrix object
    m <- solve(data, ...)  ## do the actual inversion.
    x$setinv(m) ## build the cache
    
    ## Return a matrix that is the inverse of 'x'. Note that the return type is matrix and not 
    ## any special object.
    m
}
