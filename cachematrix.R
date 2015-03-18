## makeCacheMatrix function returns a list containing the following functions:
## 1) set - sets the value of the matrix
## 2) get - returns the value of the matrix
## 3) setSolve - sets the inverse of the matrix
## 4) getSolve - returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setSolve <- function(solve) m <<- solve
    
    getSolve <- function() m
    
    list(
        set = set,
        get = get,
        setSolve = setSolve,
        getSolve = getSolve
    )
}


## cacheSolve calculates the inverse of the matrix created with makeCacheMatrix
## It first checks to see if the inverse has already been calculated. If so,
## it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of 
## the matrix in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    
    mtrx <- x$get()
    
    m <- solve(mtrx, ...)
    
    x$setSolve(m)
    
    m
}
