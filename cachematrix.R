
makeCacheMatrix <- function(m = matrix()) {
    #Reset Matrix Inverse
    xInvertd <- NULL
    #Set the cache input
    set <- function(xVal) {
        m <<- xVal
        xInvertd <<- NULL #Reset
    }
    get <- function() m;
    setInverse <- function(invertdata) xInvertd <<- invertdata
    getInverse <- function() xInvertd
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# This function will retrieve the 
# cached matrix if inverted
cacheSolve <- function(m, ...) {
    x <- m$getInverse()
    if(!is.null(x)) {
        # If the data is set    
        message("getting cached data")
        return(x)
    }
    output <- m$get()
    x <- solve(output, ...)
    m$setInverse(x)
    #Matrix Inverse
    x
}