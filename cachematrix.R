## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix creates a special "matrix", which is really a list containing a function to:

#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse of a matrix
#4. get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmean <- function(invmatrix) inv <<- invmatrix
  getmean <- function() inv
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## Function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
#  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#  retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getmean()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setmean(inv)
  inv	
}
