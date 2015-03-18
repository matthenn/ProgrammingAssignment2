# Explanations:

## makeCacheMatrix takes the matrix as input, and creates a list of function objects.
## One of these, setinverse, will actually calculate the value of the matrix's inverse, 
## while the others simply store the existing parameters as functions. This allows the
## next function to determine whether or not it will have to calculate the inverse of the 
## matrix, based on whether i exists or not, and to be able to calculate the value if 
## i is null. 

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


## cacheSolve takes the result of the makeCacheMatrix function (a list of function objects), 
## and establishes i as the inverse of the matrix calculated from the previous function. 
## If i was calculated, then the function simply prints i, saving computation. If i wasn't 
## calculated, it gets the data from the previous function and calculates the inverse. 

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
