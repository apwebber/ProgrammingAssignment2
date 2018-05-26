## The functions makeCacheMatrix and cacheSolve together calculates the inverse
## of a matrix and caches them. The inverted matrix can then be retrieved, or
## recalculated using a different matrix

## makeCacheMatrix creates an object that stores the matrix, the cached
## matrix (when calculated), and the functions that allow all this magic
## to happen.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInv <- function(inverted) inv <<- inverted
    
    getInv <- function() inv
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve returns the inverted matrix defined by the object created by
## makeCacheMatrix. It calculates it if needed, stores it in the cache, or
## or retrives it from the cache if it already exists.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        
        if(!is.null(inv)) {
            message("getting cached data...")
            return(inv)
        }
        
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$setInv(inv)
        
        inv
}
