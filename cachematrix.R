## Below are two functions that are used to create a special object
## that stores a square invertible matrix and caches its inverse

##The first function, makeCacheMatrix creates a special list
## of functions that:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function first checks to see if the inverse matrix has
## already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates
## the inverse and sets its value in the cache via the setsolve
## function.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}