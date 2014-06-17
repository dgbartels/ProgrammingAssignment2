
## makeCacheMatrix and cacheSolve make it possible to store and 
## retrieve a user defined matrix (assumed to be invertible),
## and either 1) compute its inverse, if it hasn't already, or
##            2) retrieve its inverse, if it has been cached.
##
##      try:
##      myMatrix <- matrix(rnorm(4), 2, 2)
##      m <- makeCacheMatrix()
##      m$set(myMatrix)
##      m$get()
##      cacheSolve(m)
##      cacheSolve(m)


## makeCacheMatrix() constructs 4 functions:
##      set() & get() for the user
##      setinv() & getinv() called by cacheSolve()

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
}


## cacheSolve checks to see if the matrix inverse has been
## cached already. If it has, it returns the cached matrix,
## otherwise, it computes, stores and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached matrix inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
