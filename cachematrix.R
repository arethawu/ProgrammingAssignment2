## The following functions caches the inverse of a matrix

## This function takes in a matrix as input and returns a list with
## properties to return the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes in the object created by the first function and 
## computes the inverse of the matrix if it hasn't been computed already.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  tmp_inv <- x$getinverse()
  if(!is.null(tmp_inv)) {
    message("getting cached data")
    return(tmp_inv)
  }
  data <- x$get()
  tmp_inv <- solve(data, ...)
  x$setinverse(tmp_inv)
  tmp_inv
}
