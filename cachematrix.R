library(matrixcalc)
## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix is a function that stores a list of functions

## invx variable name of the value of matrix inverse
## Write a short comment describing this function
## get is a function that returns the vector x stored in the main function.
## set is a function that changes the vector stored in the main function.
## setInverse  set the value of matrix inverse
## getInverse get the value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  get <- function() {
    return(x)
  }
  set <- function(y) {
    x <<- y
  }
  setInverse <- function(inverseX)
    invx <<- inverseX
  getInverse <- function()
    invx
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
  
  
}


## Write a short comment describing this function
##calculate and store the inverse for the input argument if it is of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx = x$getInverse()
  
  if (!is.null(invx)) {
    message("getting Inverse data")
    return(invx)
  }
  
  data <- x$get()
  
  if (!is.square.matrix(data)) {
    message("this matrix not squared")
    return(data)
  }
  
  invx <- solve(data)
  x$setInverse(invx)
  invx
}
