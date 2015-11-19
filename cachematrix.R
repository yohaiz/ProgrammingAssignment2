## Assignment 2 - R programing
## Function to demonstrate working with lexical scoping rules

## Function defining a new "type" of matrix capable of cashing the value of it's inverse

makeCacheMatrix <- function(x = matrix()) {
  ## implmentation folows the lines of makeVector example
  invMatVal <- NULL ## variable to hold matrix inverse
  set <- function(y) {## function to set matrix and reset inverse
    x <<- y
    invMatVal <<- NULL
  }
  get <- function() x ## returns the matrix
  setinv <- function(invVal) invMatVal <<- invVal ## sets the value of the inverse
  getinv <- function() invMatVal ## gets the vakue of the inverse
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv) ##returns the list of the internal functions
}


## Function to calculate the inverse and store it for future calls

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invVal <- x$getinv() ##reads current inverse
  if(!is.null(invVal)) { ##if the inverse is cached
    message("getting cached data")
    return(invVal) ##returns the cached inverse
  } ##else
  data <- x$get() ##gets the matrix
  invVal <- solve(data, ...) ##solves the inverse
  x$setinv(invVal) ##stores in cache for future calls
  invVal ##returns inverse
}
