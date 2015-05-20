## creates a "CacheMatrix" that encapsulates the matrix x and its inverse i
## the CacheMatrix is a list of getter/setter functions
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## returns the inverse of a matrix encapsulated in a CacheMatrix object

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(is.null(inverse)) {
    #the inverse matrix has not been calculated yet 
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)

  }
  #now the inverse matrix should be ok
  inverse
}
