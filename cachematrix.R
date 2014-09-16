## Using the two methods below, the calculation of the inverse of a 
## matrix can be cached. A matrix object created by makeCacheMatrix 
## will store the inverse internally. Method cacheSolve will return
## the cached value if it already exists.

## Create a "matrix object" with caching functionaliy in the closure

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Get the inverse of a matrix object, from cache or calculated

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
