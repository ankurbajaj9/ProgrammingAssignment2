## The following set of functions enable the caching of inverse of a function. to use it 
## pass the matrix whose inverse is to be created into makeCacheMatrix. the object that 
## is returned from the first function is passed into the cacheSolve function. this function
## checks if the inverse is already calculated. if yes, return the cached inverse otherwise
## create the inverse using solve function. cache the solution and return it

## makeCacheMatrix returns a list which contains the setter-getter of the matrix as well as
## its inverse. it resets the inverse if the matrix is changed using the setter

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve returns the inverse of the matrix using the object returned by the makeCacheMatrix
## function. It would create the inverse of the matrix if it does not already exist or return  
## the value if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
