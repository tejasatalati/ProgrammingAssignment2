## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {

  ## To Store cached inverse of the matrix
  inv <- NULL
  
  ## Indicate if matrix has changed
  matChanged <- TRUE
  
  ## set the value of the matrix
  setMatrix <- function(x) 
  {
    m <<- x
    inv <<- NULL
    matChanged <<- TRUE
  }
  
  ## Get the current set matrix
  getMatrix <- function() m
  
  ## Set the calculated inverse of a matrix, and indicate that the matrix is not changed
  setInverse <- function(inverse) 
  {
    matChanged <<- FALSE
    inv <<- inverse
  }
  
  ## Get the inverse of the matrix
  getInverse <- function() inv
  
  ## Returns whether the matrix has changed or not
  isMatChanged <- function() matChanged
  
  ## Return the list of functions
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse,
       isMatChanged = isMatChanged)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  ## Get the cached inverse of matrix
  inv <- x$getInverse()
  
  ## CHeck whether the matrix has changed or not
  isMChanged <- x$isMatChanged()
  
  ## If the cached inverse is not NULL and if the matrix has not changed
  
  if(!is.null(inv) && !isMChanged) 
  {
    message("Getting cached inverse of matrix")
    return(inv)
  }
  
  ## Get the current matrix for which inverse has been requested
  mt <- x$getMatrix()
  
  ## Calculate the inverse of matrix using 'solve' function.
  inv <- solve(mt, ...)
  
  ## Set the calculated inverse 
  m$setInverse(inv)
  
  message("Getting fresh calculated inverse of matrix")
  
  ## Return Inverse of the matrix
  inv
  
}
