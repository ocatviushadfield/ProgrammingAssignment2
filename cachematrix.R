## These two functions allow for the efficient computation of an inverse matrix, in the case where it may be required mutliple times.
## Rather than calculating it afresj each time it's needed, the inverse is 'cached', and pulled for re-use if required.
##
## There is no elegant error-handling; the functions do not test that the matrix has an inverse
##


## The makeCacheMatrix function creates a special object that stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
## returns a list containing functions to
##      - set the matrix
##      - get the matrix
##      - set the inverse
##      - get the inverse
##
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- mean
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function uses the 'solve' function to compute the inverse of the matrix created
## by makeCacheMatrix.

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

            
## The functions can be demonstrated thus:
##  test_matrix <- c(2,3,5,7)
##  dim(test_matrix) <- c(2,2)
##  x <- makeCacheMatrix(test_matrix)
##  cacheSolve(x)


## The following, copied from the R-Studio console, show this in action:
## > test_matrix <- c(2,3,5,7)
## > dim(test_matrix) <- c(2,2)
## > test_matrix
##      [,1] [,2]
## [1,]    2    5
## [2,]    3    7
## > solve(test_matrix)
##      [,1] [,2]
## [1,]   -7    5
## [2,]    3   -2
## > x <- makeCacheMatrix(test_matrix)
## > cacheSolve(x)
##      [,1] [,2]
## [1,]   -7    5
## [2,]    3   -2
## > 
