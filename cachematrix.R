## A pair of functions that cache the inverse of a matrix.



##The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:

##    set the value of the matrix
##    get the value of the matrix



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

##The following function calculates the inverse of special “matrix” created with the above function. However, it first checks 
##to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the mean of the data and sets the value of the inverse in the cache via the cacheSolve function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

