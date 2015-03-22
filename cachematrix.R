## This R file can cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Assign the input matrix to a local copy 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Creating a cached matrix
  get <- function() x
  ## Setting the inverse of the matrix
  setinverse <- function(inverse) m <<- solve
  ## Creating a cached inverse of the matrix
  getinverse <- function() m
  
  ## Making the special 'matrix' object
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
