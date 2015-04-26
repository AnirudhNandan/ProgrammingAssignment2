## makeCacheMatrix function creates a list containg 4 functions:
## 1. set(), 2. get(), 3. setinv(), 4. getinv()
## inv is set to NULL so that when makeCacheMatrix is called for the first time ther is no stored inverse
## value of the data and inv are set in set() method of this function
## value of x can be obtained by calling get() method
## computed inverse can be stored by calling setinv() method
## previously computed inverse can be obtained by calling getinv() method
## Eg. calling: x <- makeCacheMatrix(matrixS) where matrixS is some invertible square matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## this function first gets the inverse of 'x'
## if inv has a value this function will obtain it and display what it does
## if not data is obtained from 'x' by get() method
## inverse of the data is computed
## the newly computed inverse is placed in cache by setinv() method
## the newly computed inverse is returned
## Eg. calling: cacheSolve(x)
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
