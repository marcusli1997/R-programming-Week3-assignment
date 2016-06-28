## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	iv <- NULL
  set <- function(y){
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) iv <<- inverse
  getInverse <- function() iv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	iv <- x$getInverse()
  if (!is.null(iv)) {
    message("getting cached data")
    return(iv)
    }
  mat <- x$get()
  iv <- solve(mat, ...)
  x$setInverse(iv)
  iv
        ## Return a matrix that is the inverse of 'x'
}
