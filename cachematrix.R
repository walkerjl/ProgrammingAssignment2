## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL               ##initialize matrix to NULL
    set <- function(y) {    ##set the value of the matrix
      x <<- y
      m <<- NULL
    }
    get <- function() x     ##get the value of the matrix
    setinverse <- function(solve) m <<- solve  ##set the value of the inverse
    getinverse <- function() m                   ##get the value of the inverse
    list(set = set, get = get,                   ##list all functions created
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()      ##check cache for inverse
    if(!is.null(m)) {        ##if cached inverse is available, return matrix and exit function 
      message("getting cached data")
      return(m)
    }
    data <- x$get()          ##cached matrix not available - get the matrix
    m <- solve(data, ...)    ##compute inverse
    x$setinverse(m)          ##cache inverse
    m                        ##return inverse
}

