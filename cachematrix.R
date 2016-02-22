## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## X is a square invertible matrix
  ## makeCacheMatrix function a matrix, which is really a list of functions
  ##  1. Set the matrix
  ##  2. Get the matrix
  ##  3. Set the inverse matrix
  ##  4. Get the inverse matrix
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("Getting cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}