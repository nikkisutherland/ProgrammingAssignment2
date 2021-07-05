## To use these functions, you need to call makeCacheMatrix() first and then
## call cacheSolve(). For example:
##
##    x <- matrix(c(11,2,10,2), nrow = 2, ncol = 2)
##    m1 <- makeCacheMatrix(x)
##    cacheSolve(m1)


## This function creates a matrix to cache the result of a solve operation
## and provides the user with methods to access and set both the matrix and
## the cached solve result

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
  
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
  
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m  
  
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of a matrix. If the inverse is stored
## in the matrix cache, then the function returns this result. If the inverse
## has not yet been calculated, this function calculates the inverse and then
## then stores it in the cached matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
  
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
  
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m  
}
