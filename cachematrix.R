## The functions store a matix and cache the inverse of the matrix

## This function creates a Matrix which can cache its inverse
## it first sets and gets the value of a matrix and then sets and gets the inverse one.

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


## This function returns the inverse of the matrix and checks if it is computed.

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
