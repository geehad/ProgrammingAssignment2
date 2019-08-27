## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse_mat) inv_mat <<- Inverse_mat
  getInverse <- function() inv_mat
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inverse_mat <- x$getInverse()
  if(!is.null(Inverse_mat)) {
    message("getting cached data")
    return(Inverse_mat)
  }
  data <- x$get()
  Inverse_mat <- solve(data)
  x$setInverse(Inverse_mat)
  Inverse_mat
}

