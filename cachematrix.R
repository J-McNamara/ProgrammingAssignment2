## This assignment illustrates the use of
## lexical scoping and manipulating cached
## variables to avoid duplicating costly
## calculations.

##########
## Create a list containing functions to
## set a matrix value, get that value, set
## the matrix's inverse, and get the matrix
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
    set <- function(y) {
        x <<- y
        matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matrix <<- inverse
    getinverse <- function() matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

###########
## Check the cache for matrix inverse. If it is not cached,
## calculate matrix inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrix <- x$getinverse()
    if(!is.null(matrix)) {
        message("getting cached matrix inverse")
        return(matrix)
    }
    else {
    data <- x$get()
    matrix <- solve(data, ...)
    x$setinverse(matrix)
    return(matrix)
    }
}