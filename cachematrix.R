## A pair of functions that cache inverse of a matrix

## makeCacheMatrix create a matrix that can cache its inverse
## The function return a list of functions that can get the value of matrix
## and its cached inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ## set: set the value of:
    ##      (1) x as input matrix, and 
    ##      (2) variable i as a NULL variable to contain inverse of matrix x
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## get: return the value of input matrix
    get <- function() x
    
    ## setinverse: set the value of inverse
    setinverse <- function(solve) i <<- solve
    
    ## getinverse: return the value of inverse
    getinverse <- function() i
    
    ## return list of above functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve get the inverse of matrix returned from above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    ## Check if inverse has been calculated.
    ## If it has been calculated then return the value of inverse.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## If inverse hasn't been calculated then calculate inverse of x
    message("calculate inverse and cache the value")
    data <- x$get()         ## get input matrix
    i <- solve(data, ...)   ## calculate inverse of matrix 
    x$setinverse(i)         ## cache the value of inverse
    i                       ## return the inverse
}
