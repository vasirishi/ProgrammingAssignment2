#######################################################
## Program Name: cachematrix.R
## Author: Kannan Subbiah
## Date: July 16 2014
## Version: 1.0.0
#######################################################

#######################################################
## Function :   makeCacheMatrix
## Arguments:   x , which is a square invertible Matrix
## Returns  :   A List
#######################################################

makeCacheMatrix <- function(x = matrix()) {
    cache   <<- NULL
    setdata <- function(y) {
        # Assign matrix to 'x', initialize 'inverse' and 'cache' to NULL
        x       <<- y
        inverse <<- NULL
        cache   <<- NULL
    }
    # getdata() returns the entire matrix
    getdata     <- function()       x
    
    # getinverse() returns the inverse of the matrix
    getinverse  <- function()       inverse 
    
    # setinverse() sets the inverse of the matrix
    setinverse  <- function(Value)  inverse <<- Value
    
    # getcache() returns the whether the inverse is in cache or not
    # NULL if not in cache, 1 if it is in cache
    getcache    <- function()       cache
    
    # setcache() sets the cache value
    setcache    <- function(Value)  cache <<- Value
    
    list(setdata    = setdata, 
         getdata    = getdata,
         getinverse = getinverse,
         setinverse = setinverse,
         getcache   = getcache,
         setcache   = setcache)
}


#######################################################
## Function :   cacheSolve
## Arguments:   x - A square invertible Matrix
## Returns  :   A Matrix which is Inverse of 'x'
#######################################################
cacheSolve <- function(x, ...) {

    if ( is.null( x$getcache() ) ) {
        # Check cache value for NULL, if so Inverse is not in cache
        # Get the Matrix from cache
        data <- x$getdata()
        
        # Calculate the inverse and store it back using setinverse function()
        x$setinverse(solve(data,...))
        
        # Set the cache value to 1, so that next time, we can pull from cache.
        x$setcache(1)
    } else {
        # getcache returned 1, which means Inverse is in Cache
        # Print message and follow code path to return the inverse value
        message("Getting Inverse from cache...!")
    }
    # Return the inverse
    return (x$getinverse())
}
