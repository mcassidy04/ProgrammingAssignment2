## Provides cached storage for matrix and its
## inverse as well as a list of functions used
## to manage the cached objects.

## Function takes invertible matrix as argument. 
## Returns list of functions to get/set matrix and
## its inverse.
## Stores matrix 
## in parent or global frame

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function takes a makeCacheMatrix() object as argument. Returns
## inverse of matrix cached. On first run, it will 
## solve and store inverted matrix; on subsequent calls will return 
## cached inverted matrix rather than recalculating.
## (Assumes cached matrix is invertible.)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    message("generating and returning inverse of data")
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

