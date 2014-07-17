## Functions are used to create a matrix and cache the inverse.

## makes a list of functions

makeCacheMatrix <- function(x = matrix()) {
    
    m = NULL
    
    set <- function(y){
        
        x <<- y
        
        m <<- NULL
    }
    get <- function() x
    
    setsolve <- function(solve) m <<- solve
    
    getsolve <- function() m
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## solves the inverse of a matrix created in makeCacheMatrix but first 
## checks to see if inverse have already been calculated if so the cache
## is returned

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    
    if(!is.null(m)){
        
        message("getting cached data")
        
        return(m)
    }
    
    data <- x$get()
    
    m <- solve(data, ...)
    
    x$setsolve(m)
    
    m
    
}

