## Put comments here that give an overall description of what your
## functions do

## takes the matrix as input, and creates a list of function objects
## (one of these will actually calculate the value of the matrix's inverse, 
## the others simply store the existing paramters as functions)
## that the next function will be able to use to determine whether or 
## not it will have to calculate the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## establishes i as the inverse of the matrix calculated from the previous function. 
## if i was calculated, then this one simply prints i. if i wasn't calculated, it gets
## the data from the previous function and calculates the matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
