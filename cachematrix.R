## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  inverse <- NULL
  
  ## getter/setter for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## getter/setter for matrix inverse
  getinverse <- function() inverse
  setinverse <- function(inverse) inverse <<- inverse
  
  ## return list of functions for matrix
  list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  # return cached matrix inverse if it's been already computed
  if (!is.null(inverse)) {
    message("inverse is cached")
    return(inverse)
  }
  
  # compute inverse of matrix 
  m <- x$get()
  inverse <- solve(m, ...)
  
  # cache inverse
  x$setinverse(inverse)
  
  # return inverse of matrix
  return(inverse)
}
