## Put comments here that give an overall description of what your
## functions do

  # 1. makeCacheMatrix creates a square matrix that can cache its inverse
  # 2. cacheSolve computes the inverse of the matrix returned by makeCacheMatrix, or retrieves it
  #     if it has already been calculated and cached in the makeCacheMatrix

## Write a short comment describing this function
  # defines a sqare matrix
  # defines an inverse of x ("inverse") and sets it to NULL

  # (1)
  # sets the value of the matrix x by attributing it new values (<<- y)
  # re-sets the inverse of x to NULL

  # (2)
  # gets the original value of the matrix x

  # (3)
  # computes the inverse of matrix x using function solve(x) and caches it to "inverse"

  # (4)
  # gets the value of the "inverse"

  # defines a list with the above four functions so they can be called in cacheSolve
  
makeCacheMatrix <- function(x = numeric(), size, nrow = size, ncol = size) {
length(x) <- size^2
  dim(x) <- c(size,size)
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setSolve <- function(solve) inverse <<- solve(x) 
  
  getSolve <- function() inverse
  
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Write a short comment describing this function
  
  # calculates inverse matrix of x using getSolve function from makeCacheMatrix
  # checks if inverse matrix empty
    # if empty, calculates new inverse matrix
    # if not empty, gets cached data
  # returns inverse matrix of x
  
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
 inverse <- x$getSolve()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setSolve(inverse)
  inverse        
}
