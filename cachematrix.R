## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinv <- function(inverse) {
    inv <<- inverse
  }
  
  getinv <- function() {
    inv
  }
  
  list (set = set,
        get = get,
        setinv = setinv,
        getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  
  data <- x$get()
  
  if (nrow(data) != ncol(data)) {
    message("matrix must be square")
    inv <- NaN
  }
  else {
    D <- det(data)
    if (D != 0) {
      inv <- solve(data, ...)
    }
    else {
      message("matrix has non-zero determinant -> cannot be inversed")
      inv <- NaN
    }
  }
  
  x$setinv(inv)
  
  inv
          ## Return a matrix that is the inverse of 'x'
}
