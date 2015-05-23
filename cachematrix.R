## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##function that returns a special matrix object that can chache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
  ## initializing inverse matrix
  matInverse <- NULL
  ## function to set matrix
  setMatrix <- function(y) {
    x <<- y
    matInverse <<- NULL
  }
  ## function to get matrix
  getMatrix <- function() x
  
  ## function to set inverse matrix in cache
  setInverse <- function(matInverse) matInverse <<- matInverse
 
  ## function to get inverse matrix from cache
  getInverse <- function() matInverse
  
  ## creating and returning function list
  functionList <- list(setMatrix = setMatrix, getMatrix = getMatrix,
        setInverse = setInverse,
       getInverse = getInverse)
  
}
 
##function that computes the inverse of special matrix if the inverse does not exist in chache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## call function for getting inverse from cache
  matInverse <- x$getInverse()
  ## check if inverse exists in cache and  Return if exists
  if(!is.null(matInverse)) {
    message("Returning matrix from cache")
    return(matInverse)
  }
  
  ## if inverse does not exist in cache then get matrix
  data <- x$getMatrix()
  ## calculate inverse of matrix
  matInverse <- solve(data, ...)
  ## sets the inverse 
  x$setInverse(matInverse)
  ## returns the inverse 
  matInverse
  
  
}
