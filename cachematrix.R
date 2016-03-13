## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# function aims at caching the inverse of a matrix object
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  # setinverse <- function(solve) m <<- inverse
  setinverse <- function(inverse) inv <<- inverse
  #getmatrix <- function() inv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
# this fuction calculates the inverse of the "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  matrix_inv <- x$get()
  inv <- solve(matrix_inv, ...)
  x$setinverse(inv)
  inv
}
