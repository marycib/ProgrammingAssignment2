## Put comments here that give an overall description of what your
## functions do

##with this function we will make an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInver <- function(inverse) inv <<- inverse
    
    getInver <- function() inv
    
    list(set = set,get = get, setInver = setInver, getInver = getInver)   
    
    
    
    

}


## this is the function to store the cache memory

cacheSolve <- function(x, ...) {
    
    
        ## Return a matrix that is the inverse of 'x'
    
    
    inv <- x$getInver()
    
    if (!is.null(inv)) {
        
        message("getting cached data")
        
        return(inv)
    }
    
    dt <- x$get()
    
    inv <- solve(dt, ...)
    
    x$setInver(inv)
    
    inv
    
    
    
}
