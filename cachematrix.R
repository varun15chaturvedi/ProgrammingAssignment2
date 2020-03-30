## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creating a new matrix object which has special Apttribute to it 
makeCacheMatrix <- function(x = matrix()) {
  ##inverse of Matrix is stored in inverse variable 
  inverse <- NULL
  ## setting matrix value and its inverse to null
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #getting matrix value
  get <- function() x
  ##setting matrix inverse
  setinverse <- function(inverse) inverse <<- inverse
  ## getting matrix inverse value
  getinverse <- function() inverse
  ## returning a list of all attributes of new matrix object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## checking is there already a inverse Exists
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## if not then getting Matrix and finding inverse.
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

