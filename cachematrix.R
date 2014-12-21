## https://github.com/mattsteelexx/ProgrammingAssignment2.git
## R Programming Assignment 2: Lexical Scoping

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
  {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) n <<- inverse
  getinverse <- function() n
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
  }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
  {
  ## Return a matrix that is the inverse of 'x'
  n <- x$getinverse()
  if(!is.null(n)) {
    return(n)
  }
  matrixdata <- x$get()
  n <- solve(matrixdata, ...)
  x$setinverse(n)
  n
  }
