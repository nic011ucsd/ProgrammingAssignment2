## Matrix inversion is usually a costly computation. 
## The function below are used to create a special 
## object that stores the inverse of matrix and caches.

## This function creates a special matrix that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() {
        inver <- getInverse(x)
        inver%%x
  }
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special 
## matrix created by function "makeCacheMatrix" above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

w <- makeCacheMatrix(matrix(1:10, 2, 5))
w$get()

w$getInverse()
cacheSolve(w)
