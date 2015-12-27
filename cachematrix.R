## makeCacheMatrix - Accepts a matrix data type as its input and returns a list of functions that performs 
##    get, set, get Inverse and set Inverse on the input matrix.
##
## CacheSolve - Accepts a cacheMatrix data type created using "makeCacheMatrix". If the inverse of a matrix has 
##    not been calculated, it sets the calculates the inverse of the matrix and sets it. 
##    If the inverse matrix has already been calculated, it will return the cached matrix

## functions do

## makeCacheMatrix - Accepts a matrix data type and returns a list of getters and setters functions on the matrix data.

makeCacheMatrix <- function(x = matrix()) {
  myMatrix <<- x
  myInverseMatrix <<- NULL
  #returns matrix
  get <-function() myMatrix
  
  #sets matrix
  set <- function(newMatrix)
  {
    myMatrix <<- newMatrix 
    #reset inverse matrix, so that cacheSolve will recalculate
    myInverseMatrix <<- NULL
  }
  
  #returns inverse matrix
  getInverse <- function() myInverseMatrix
  
  #sets inverse matrix
  setInverse <- function(inverseMatrix) myInverseMatrix <<-inverseMatrix
  
  #returns list of functions
  list(get=get, set=set, getInverse = getInverse, setInverse=setInverse)
}


## cacheSolve - Solves for the inverse of the matrix created using 'makeCachedMatrix'. Returns cached results if it exists.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(!is.null(x$getInverse()))
  {
    #print cached when taken from cache
    print('Cached')
    return(x$getInverse())
  }
  else
  {
    inverseTemp <- solve(x$get())
    x$setInverse(inverseTemp)
    inverseTemp
  }
}
