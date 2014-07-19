## The makeCacheMatrix and cacheSolve functions are designed to cache potentially time-consuming computation of inverse matrix.
## By chaching the inverse matrix result we won't need to compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse. Tis object can be used by:
##      set(y) function setting the matrix to be inverted to y
##      get() function to return the matrix
##      setinv(inverse) function to change the inversion cached to inverse
##      getinv() function to return the cached inversion
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache otherwise inverse is computed and cached.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("returning inverse matrix cached previously")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}






