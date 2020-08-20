## This function creates a special "matrix" object that can cache its inverse


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #Get the matrix
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  #this list is used as the input to cacheSolve()
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  # if the inverse has already been calculated
  if (!is.null(i)) {
    message("getting data from the cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
