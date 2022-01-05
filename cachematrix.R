##Overall this assignment aims at calculating and making a caxhe matrix and then making a function to retrive it whenever we require insteadof solving it again and again
## Here we make the special "matrix" that can be cached for further use as it is a costly computation
## we set and get the values and the set and get the inverses of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##This function aims to compute the inverse of the matrix returned by makeCacheMatrix that we have typed above.
##Incase of the situation where the inverse has already been calculated, then cacheSolve will  retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
