## This R file can cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ##returns a special "matrix" object which is a list containing the function to set the value of the matrix, get its value, set the inverse of the matrix and get its value too.
  m <- NULL 
  set <- function(y) {
    ##sets the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x ## gets the value of the matrix
  setinverse <- function(inverse) m <<- solve ##calculates the inverse
  getinverse <- function() m ##gets the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #Getting inverse from the cache
  m <- x$getinverse()
  ## Check if cache exists, return the cached value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## IF NOT , then get the matrix and find the inverse using solve function
  data <- x$get()
  m <- solve(data, ...)
  ## Set the inverse matrix in the special matrix 'object' 
  x$setinverse(m)
  m
}

