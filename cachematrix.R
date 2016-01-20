#  vim: set ts=4 sw=4 sts=4 tw=80 et cc=80 :

## Returns a special "vector" representing a
## matrix with cacheble inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inv <<- i
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of a matrix, either the
## cached version (if available), or the newly computed one
cacheSolve <- function(x) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    i <- solve(mat)
    x$setinverse(i)
    i
}
