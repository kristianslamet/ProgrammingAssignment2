## The overall purpose of this function is to
## firstly create a matrix with caching capability
## using the makeCacheMatrix function,
## and then return the inverse matrix
## using the cacheSolve function

## Note: this assumes the matrix has an inverse, which is not always true.


## makeCacheMatrix is a list which stores
## 4 functions, set, get, setInverse and
## getInverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve first checks whether the inverse
## matrix has been computed. If yes, cacheSolve
## will return the cached inverse matrix.

## Otherwise, cacheSolve will compute the
## inverse matrix using solve function,
## perform caching on the solved inverse matrix,
## then return the inverse matrix to the user.

cacheSolve <- function (x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached inversed matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

## How to test the function
## > newMatrix <- matrix(c(1,3,2,4), nrow=2,ncol=2)
## > newMatrix
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## > cachedMatrix <- makeCacheMatrix(newMatrix)
## > cacheSolve(cachedMatrix)
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
## Comment: the second time cacheSolve is called,
## Comment: the cached value will be called.
## > cacheSolve(cachedMatrix)
## getting cached inversed matrix
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5