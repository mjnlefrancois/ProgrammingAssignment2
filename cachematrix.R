## This function that can cache the inverse of a matrix object
## If is based on the example makeVector provided for this assignment.
## "set" and "setinversion" are used to assign a value to an object in 
## an environment that is different than the current environment (using <<)

## For example:
##
## > z <- makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,4,0),nrow=3,ncol=3))
## 
## The first time we call cacheSolve:
##
## > cacheSolve(z)
##       [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0
##
## we get a result that has been calculated. However,
## when we call it again, the inverse matrix is retrieved
## from cache:
##
## > cacheSolve(z)
## getting cached data
##       [,1] [,2] [,3]
## [1,]   -24   18   5
## [2,]    20  -15  -4
## [3,]    -5    4   1

## This function, makeCacheMatrix, creates a special "matrix", 
##  which is really a list containing a function to:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinversion <- function(inversion) m <<- inversion
    getinversion <- function() m
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)
}


## This function calculates the inverse of the matrix that is returned 
## by makeCacheMatrix. When the inverse was already calculated, then 
## cacheSolve does retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    
    m <- x$getinversion()
    if(!is.null(m)) {                    ## Don't solve again but get cached matrix instead
        message("getting cached data")
        return(m)
    }
    data <- x$get()                      ## Never been cached before - need to set the inverse matrix
    m <- solve(data, ...)
    x$setinversion(m)
    m
}