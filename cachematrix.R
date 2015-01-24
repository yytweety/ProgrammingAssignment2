## the following two functions provide a cache version of the 
## solve function for computing the inverse of a matrix.
## usage:
## makeCacheMatrix(x):  create a special "matrix", whose inverse will be
##                    looked up from cache rather than recomputed, if 
##                    it has been computed before
## cacheSolve(x, ...): return the inverse of the x

##
## makeCacheMatrix
## this function creates a special "matrix",
## which is a list containing a matrix and functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list( set = set, get = get, 
                setinverse = setinverse, 
                getinverse = getinverse )
}

##
## cacheSolve
## this function returns the inverse of the special matrix 
## created by makeCacheMatrix(x)
## - it computes the inverse using solve(x) and save the result,
##   if it's not computed before
## - otherwise, it will retun the saved result without recomputation
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
