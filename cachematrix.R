## Put comments here that give an overall description of what your
## functions do

## Return to a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
    
    invMat <- NULL
    set <- function(y) {
        x <<- y
        invMat<<-NULL
    }
    get <- function() x
    setInv <- function(sol) invMat <<- sol
    getInv <- function() invMat
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
    

}


## Solve the inverse matrix after searching if it has already been solved


cacheSolve <- function(x, ...) {
    invMat <- x$getInv()
    
    if(!is.null(invMat)) {
        message("getting cached data")
        return(invMat)
    }
    
    data <- x$get()
    
    invMat <- solve(data, ...)
    x$setInv(invMat)
    
    invMat
    
}
