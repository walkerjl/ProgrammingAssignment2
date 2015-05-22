## A pair of functions to cache the inverse of a matrix

## Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                                  ##initialize matrix to NULL
    set <- function(y) {                       ##set the value of the matrix
      x <<- y
      m <<- NULL
    }
    get <- function() x                        ##get the value of the matrix
    setinverse <- function(solve) m <<- solve  ##set the value of the inverse
    getinverse <- function() m                 ##get the value of the inverse
    list(set = set, get = get,                 ##return list of functions created
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function to compute the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed) then cacheSolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()             ##check cache for inverse
    if(!is.null(m)) {               ##if cached inverse is available, return matrix and exit function 
      message("getting cached data")
      return(m)
    }
    data <- x$get()                 ##cached matrix not available - get the matrix
    m <- solve(data, ...)           ##compute inverse
    x$setinverse(m)                 ##cache inverse
    m                               ##return inverse
}

