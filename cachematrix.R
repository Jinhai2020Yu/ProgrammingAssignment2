## The first function creates a special matrix that can cache its inverse.

## The second function will computes the inverse of a matrix created by the first function, 
## if the inverse has been already calculated, it will retrieve the results from the cache.

## Get the inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <-function() x
  setinverse <- function(inverse) Inv <<- inverse
  getinverse <- function() Inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute the inverse, if exits, retrieve from cache.

cacheSolve <- function(x, ...) {
  Inv <- x$getinverse()
  if(!is.null(Inv)) {
    message("getting from cached data")
    retrun(Inv)
  }
  matrix <- x$get()
  Inv <- solve(matrix, ...)
  x$setinverse(Inv)
  Inv
  
}
