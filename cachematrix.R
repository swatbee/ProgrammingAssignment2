## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  myMatrix <<- x
  myInverseMatrix <<- NULL
  get <-function() myMatrix
  set <- function(newMatrix)
  {
    myMatrix <<- newMatrix 
    #reset inverse matrix, so that cacheSolve will recalculate
    myInverseMatrix <<- NULL
  }
  
  getInverse <- function() myInverseMatrix
  setInverse <- function(inverseMatrix) myInverseMatrix <<-inverseMatrix
  list(get=get, set=set, getInverse = getInverse, setInverse=setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(!is.null(x$getInverse()))
  {
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
