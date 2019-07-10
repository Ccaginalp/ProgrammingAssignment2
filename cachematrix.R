## First run makeCacheMatrix to create a matrix object
## Then run cacheSolve on it to cache on the first try and
## return the cached value on subsequent attemps.

## Makes a special matrix object that can hold the inverse in the cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function() m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Checks if the cached matrix exists, and if not, creates it

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("Retrieving cached data!")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse()
  m
  ## Return a matrix that is the inverse of 'x'
}