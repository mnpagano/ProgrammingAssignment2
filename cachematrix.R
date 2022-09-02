## These functions cache the inverse of a square matrix and compute the inverse 
## if it is not already cached.


## The makeCacheMatrix function creates an object containing a list of functions
## that set or get the contents of a matrix or its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## NULL is initially assigned to the inverse matrix "i" for later use in the function. 
  i <- NULL
  
  ## The set function is defined: The <<- operator is used to assign the value or 
  ## matrix "y" to the x object in the parent environment. NULL is assigned to the 
  ## inverse matrix "i" in the parent environment.This replaces previously set matrices.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## The get function is defined to retrieve the matrix.
  get <- function() x
  
  ## The setinverse function is defined to solve the inverse matrix. The <<- 
  ## operator is used to assign the inverse matrix to "i" in the parent environment.
  setinverse <- function(solve) i <<- solve
  
  ## The getinverse function retrieves the matrix "i".
  getinverse <- function() i
  
  ##A list object is created that names each of the four functions so that they 
  ## can be used in the rest of the program.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function checks if the inverse of a matrix has been calculated.
## - If already calculated, the function returns the inverse from the cache.
## Otherwise, the inverse is calculated.

cacheSolve <- function(x, ...) {
        ## Retrieve the inverse matrix and assign it to "i".
  i <- x$getinverse()
  ## If the inverse matrix "i" is not null, then the cached inverse matrix is retrieved. 
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ## If the inverse matrix "i" is null, then the matrix is retrieved and 
  ## the inverse is solved.
  data <- x$get()
  i <- solve(data, ...)
  ## The calculated inverse is set.
  x$setinverse(i)
  i
}
