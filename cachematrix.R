##This function will create a matrix capable of storing its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## set everything to "zero" (or null) at the start of the function
  inverse <- NULL
  set <- function(y){
    ##Don't forget that we need to use <<- because the variables are outside this environment
    x <<- y
    inverse <<- NULL
  } 
  getMatrix <- function() { ##getMatrix basically is X (the variable that has the matrix)
    x
  }
  setInverse <- function (solve){
    inverse <<- solve
  }
  getInverse <- function(){
    inverse
  }
  list (set = set, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}



## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  inverse <- x$getInverse()
  ##we check if there is already an inverse stored in x
  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$getMatrix()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
