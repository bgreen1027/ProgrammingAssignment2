## The functions within this file first provide a special matrix object that
## can cache its inverse via the makeCacheMatrix function.
## The cacheSolve function then computes the inverse of the special
## matrix returned by the makeCacheMatrix function in cases where
## it has not already been calculated. If the inverse has been calculated 
## previously, cacheSolve should retrieve the inverse from the cache.

## The makeCacheMatrix function creates a special matrix object that can cache
## its own inverse via the list of functions embedded within.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the matrix object created
## within the makeCacheMatrix function in cases where it had not already been
## calculated. If the inverse has already been calculated, the function
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
