# The function given below is use to create a special object that stores a matrix and caches of the particular matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invers <<- inverse
  getInverse <- function() invers
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function computes the inverse of the matrix created by the above function "makeCacheMatrix". 
# If the inverse already calculated then it retrieve the inverse from the cache rather than compute newly.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  invers <- x$getInverse()
  if (!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  matris <- x$get()
  invers <- solve(matris, ...)
  x$setInverse(invers)
  invers
}
