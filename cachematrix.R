## The functions below creates a special object that stores a matrix (using the 
## "<<-" operator) and then cache's its inverse. 

## This function creates a special "matrix" object that can cache its inverse. It
## returns a list of functions that
##  -  sets the value of the matrix in cache
##  -  gets the value of the matrix from cache
##  -  sets the value of the inverse in cache
##  -  gets the value of the inverse from cache
## 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of a special "matrix" object returned by 
## makeCacheMatrix function and caches. It first checks whether the inverse of 
## the matrix had been cached and retrieves the inverse if available in cache
## (so skips computation) otherwise it computes the inverse of the matrix and
## and caches it.
## 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
