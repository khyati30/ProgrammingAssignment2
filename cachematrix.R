## Put comments here that give an overall description of what your
## functions do

## The function creates a matrix object that can cache its inverse 
## how to use 
## Example 
# m <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
# m2 <- makeCacheMatrix(m)
# cacheSolve(m2)
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# inverse is cached

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


## This function computes the inverse of special matrix . If inverse is already calculated
## then it will retrieve the inverse from the cache 

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
