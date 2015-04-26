## These functions calculate the inverse of a given matrix

## This function provides the matrix data we want to solve to the WHOLE environment

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinversa <- function(solve) inv <<- solve
  getinversa <- function() inv
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}



## This function calculates the inverse of the matrix we set in the previous function

cacheSolve <- function(x, ...) {
  inv <- x$getinversa()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinversa(inv)
  inv
}
## Return a matrix that is the inverse of 'x'