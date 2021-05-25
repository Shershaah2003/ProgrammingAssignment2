## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of functions to get matrix value, set matrix value, get inverted matrix, set inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  ##assign null value to variable invert
  invert <- NULL
  ## sets the value of y to x and and invert to Null
  set <- function(y){
    x <<- y
    invert <<- NULL
  }
  ## gets the value of matrix x
  get <- function() x
  ##Assigns inverted matrix to invert (retains value in different environment)
  setInverse <- function(solveMatrix) invert <<- solveMatrix
  #Return the value stored in invert
  getInverse <- function() invert
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##Checks value in Cache and returns cache value if it is not null

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', first it checks
  ##first check if invert is null
  ## if invert is not null return cached value stored in invert
  
  invert<- x$getInverse()
  if(!is.null(invert)){
    message("getting cached data")
    return(invert)
  }
  ##if invert is null get value of x, store in variable Data
  ## use Solve function and store the value in invert
  ## pass invert to set cache data of inverse
  ## return invert
  data <- x$get()
  invert <- solve(data)
  x$setInverse(invert)
  invert      
}
