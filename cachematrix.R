## Put comments here that give an overall description of what your
## functions do

## A pair of functions that cache the inverse of a matrix

library(MASS)
## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse of the matrix
    inver<- ginv(m)
    inver%*%m
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}


#testing my function
test <- makeCacheMatrix(matrix(1:8, 2, 4))
test$get()
test$getInverse()
cacheSolve(test)

