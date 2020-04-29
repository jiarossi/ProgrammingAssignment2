## These functions calculate, cache, and return the inverse of a matrix
## https://github.com/jiarossi/ProgrammingAssignment2

## This first function creates a special matrix, which is a list containing a 
## function to set the value of the matrix, get the value of the matrix, set the 
## invese of the matrix, and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) n <<- inverse
        getinv <- function() n
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates the inverse of the matrix above in makeCacheMatrix.
## If inverse has been calculated then it will get the inverse from the cache. 
## Otherwise, it calculates inverse and sets value in the cache.

cacheSolve <- function(x, ...) {
        n <- x$getinv()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setinv(n)
        n
}
