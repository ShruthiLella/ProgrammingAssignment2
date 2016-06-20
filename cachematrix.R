## Put comments here that give an overall description of what your
## functions do

## The following functions cache the inverse of a matrix, 
## instead or repeatedly calculating the inverse, they calculate only once and cache it.

## Write a short comment describing this function
## This function creates a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
      invMat <- NULL
      set <- function(y) {
          x <<- y
          invMat <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) invMat <<- inverse
      getInverse <- function() invMat
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Write a short comment describing this function
## This function returns the inverse of the cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      invMat <- x$getInverse()
      if (!is.null(invMat)) {
              message("getting cached data")
              return(invMat)
      }
      data <- x$get()
      invMat <- solve(data, ...)
      x$setInverse(invMat)
      invMat
}
