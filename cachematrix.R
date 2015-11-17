## Caching the Inverse of a Matrix :


##  makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## x needs to ba a square matrix
        m <- NULL
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of the inverse of the matrix
        setsolve <- function(solve) m <<- solve
        ## get the value of the inverse of the matrix
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        ## checks to see if inverse of the matrix has already been calculated
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## calculates the inverse and sets the value of the inverse cache
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}