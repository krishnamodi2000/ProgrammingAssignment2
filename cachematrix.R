##Overall this assignment aims at calculating and making a caxhe matrix and then making a function to retrive it whenever we require insteadof solving it again and again
## Here we make the special "matrix" that can be cached for further use as it is a costly computation
## we set and get the values and the set and get the inverses of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##This function aims to compute the inverse of the matrix returned by makeCacheMatrix that we have typed above.
##Incase of the situation where the inverse has already been calculated, then cacheSolve will  retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

