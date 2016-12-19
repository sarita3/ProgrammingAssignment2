## These functions are a square invertible matrix. 
## It will return a list containing functions to set and get the matrix and inverse.
## makeCachceMatric will creat this list

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
         x <<- y
         inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will return the inverse of the matrix.
## This function assumes that the matric is always invertible.

cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   if(!is.null(inv)) {
      message("retrieving cached data")
      return(inv)
   }
   data <- x$get()
   inv <- solve(data)
   x$setinverse(inv)
   inv
}
