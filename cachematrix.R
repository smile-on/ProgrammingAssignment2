## Defines pair of function to create a holder and action for matrix and its inverse.
## These functions can be used to speed up repetitive inverse operations
## by caching result of the first inverse operation.
## Note, this is a toy implementation which accepts square matirx only.
## Usage:
## M  <- makeCacheMatrix(matrix({1:8},2,2))
## MI <- cacheSolve(M) # calculates inverse matrix
## MI <- cacheSolve(M) # getting cached inverse matrix


## Creates an object to hold matrix and it's inverse.
## Inverse value calculates lazzily and holds as in the cache.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invX) inv <<- invX
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" x returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
