## These functions allow for the caching of the inverse of a given matrix, with 
## the goal of making certain calculations process more quickly (especially in 
## loops).

## Create a "matrix" object with functions that allow for its inverse to be
## cached in a field (namely "i").
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Check if a "'matrix' object" has it's inverse cached. If so, return it; 
## otherwise calculate the inverse, cache it, and then return it. 
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
