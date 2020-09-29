## The functions "makeCacheMatrix" and "cacheSolve" together cache the inverse 
## of a square matrix and use the cache data for further computation.

## The function "makeCacheMatrix" creates a special "matrix" object (actually a 
## list object) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inver) inver <<- inver
        getinverse <- function() inver
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function "cacheSolve" takes in the special "matrix" returned by the 
## "makeCacheMatrix" function and computes the inverse of the matrix only if the
## inverse for the same matrix is not already in the cache. Otherwise, it just retrieves the inverse 
## from the cache and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        inver
}

