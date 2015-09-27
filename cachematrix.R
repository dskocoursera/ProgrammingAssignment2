## Create a matrix x for which the inverse calculated via solve(x) can be cached.
## Parameters:
## x - a matrix, defaults to a 1x1 NA matrix.
##
## Return: 
## A list of the following functions for manipulating the matrix
## set - set a new value as the matrix
## get - get the current value of the matrix
## getSolve - get the cached inverse, or NULL if there is no inverse cached
## setSolve - set the inverse

makeCacheMatrix <- function(x = matrix()) {
    cachedSolve <- NULL
    set <- function(y) {
        x <<- y
        cachedSolve <<- NULL ## since we've set a new matrix, clear the cached inverse
    }
    get <- function() x
    setSolve <- function(solveResult) cachedSolve <<- solveResult
    getSolve <- function() cachedSolve
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Return the inverse of the matrix x. If the inverse is cached, return the cached value,
## else calculate the inverse, cache it, and return it.
##
## Parameters:
## x - the list of functions returned from makeCacheMatrix, when the matrix was made.
## ... - extra parameters for solve()
##
## Return: 
## A matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    cachedSolve <- x$getSolve()
    if (!is.null(cachedSolve)) {
        message("getting cached data")
        return(cachedSolve)
    }
    matrix <- x$get()
    result <- solve(matrix)
    x$setSolve(result)
    result
}
