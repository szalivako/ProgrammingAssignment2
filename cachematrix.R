## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special object: Matrix with the opportunity of computing inverse
## This object stores matrix and its inverse matrix
## x -- input argument: matrix, which will be stored
## This object has for inner functions: get, set, getinv and setinv

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ## This function copies the input argument to the matrix variable and initializes the inverse
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## This function returns the original matrix
  
  get <- function() {
    x
  }
  
  ## This function copies matrix invers to the special variable
  
  setinv <- function(inverse = matrix()) {
    inv <<- inverse
  }
  
  ## This function returns the inverse of the original matrix
  
  getinv <- function() {
    inv
  }
  
  list (set = set,
        get = get,
        setinv = setinv,
        getinv = getinv)
}


## Write a short comment describing this function

## This function computes the matrix inverse or returns the cached value
## x -- input argument: special object, which can be created with "makeCacheMatrix" function
## The function returns inverse of an original matrix. It can also return NaN when original matrix is non-invertable

cacheSolve <- function(x, ...) {
  
  ## Getting the value of inverse
  
  inv <- x$getinv()
  
  ## If it is already computed, then no need to do it again. Just return it.
  
  if (!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  
  ## Getting the original matrix
  
  data <- x$get()
  
  ## Checking, whether this matrix square or not
  
  if (nrow(data) != ncol(data)) {
    message("matrix must be square")
    inv <- NaN
  }
  else {
    ## Checking the possibility of calculation inverse matrix
    ## If determinant is zero, we cannot compute the inverse (to compute determinant use function det() )
    ## Othervise computing the inverse value using function solve()
    
    D <- det(data)
    if (D != 0) {
      inv <- solve(data, ...)
    }
    else {
      message("matrix has non-zero determinant -> cannot be inversed")
      inv <- NaN
    }
  }
  
  ## After computation set the inverse value to the processed object
  
  x$setinv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  
  inv
          
}
