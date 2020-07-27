##Matrix inversion is usually a costly computation and there may be some benefit to 
##caching the inverse of a matrix rather than compute it repeatedly

##This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  l <- NULL
  set <- function(y) {
    x <<- y
    l <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) l <<- inverse
  getinverse <- function() l
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)      
}

##This function computes the inverse of the special
##"matrix" returned by makeCacheMatrix above. If the inverse has 
##already been calculated (and the matrix has not changed), then
##the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  l <- x&getinverse()
  if(!is.null(l)) {
    message("getting cached data")
    return(l)
  }
  data <- x$get()
  l <- solve(data, ...)
  x$setinverse(l)
  l
}

