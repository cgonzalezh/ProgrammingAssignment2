
## makeCacheMatrix creates a special "matrix" which is really a list.
## This object can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse)  inv<<- inverse
   getinverse <- function() inv
   list(set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
   
}


##cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
##if the inverse has already been calculated then it retrieves the inverse from
##the cache

cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   #if the condition is true the inverse has already been calculated
   if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   data <- x$get()
   #we can obtain the inverse by using solve
   inv <- solve(data, ...)
   x$setinverse(inv)
   inv
}
