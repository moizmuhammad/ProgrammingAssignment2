## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  #initialize inverse of matrix
  oMatrixInverse <- NULL
  
  #define a function for setting the given matrix and its inverse to global environment
  set <- function(y){
    oMatrix <<- y
    oMatrixInverse <<- NULL
  }
  
  #define a function for getting the matrix
  get <- function() oMatrix
  
  #define a function for setting the inverse of matrix
  setMatrixInverse <- function(x) {
    oMatrixInverse <<- x
  }
  
  #define a function for getting the inverse of matrix
  getMatrixInverse <- function() oMatrixInverse
  
  list(set=set, get=get, setMatrixInverse=setMatrixInverse, getMatrixInverse=getMatrixInverse)
}


## This functions computes the inverse of special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #check if matrix inverse exists in cache
  #if yes then return its value from cache
  oMatrixInverse <- x$getMatrixInverse()
  if(!is.null(oMatrixInverse)) {
    message('getting matrix inverse from cache...')
    return(oMatrixInverse)
  }
  
  #get the original matrix
  oMatrix <- x$get()
  
  #calculate the inverse of matrix
  oMatrixInverse <- solve(oMatrix)
  
  #cache the inverse of matrix
  x$setMatrixInverse(oMatrixInverse)
  
  #return the inverse of matrix
  oMatrixInverse
  
}
