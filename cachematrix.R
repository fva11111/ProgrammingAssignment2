## The script includes two functions for inversing a matrix. 
## The matrix should be invertible. To obviate a costly computation when 
## the inverse matrix was computed earlier the script uses cached matrix

## Creates a  matrix  object that can cache its inverse.
## It make a list of functions to set or get the invercible matrix 
## and to set (setinverse) and get (getinverse) inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  
  ## set the matrix x, erase cached inverse matrix
  set <- function(y) {
    x <<- y
    invx <<- NULL                   
  }
  ## get a matrix x from the object
  get <- function() x
  
  ## set inverse matrix after computataion
  setinverse <- function(inverse) invx <<- inverse
  
  ## get inverse matrix exists
  getinverse <- function() invx
  
  ## make an output list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get an inverse matrix from cache
  invx <- x$getinverse()
  ## if there is no the inverse matrix in cache, calculate it
  if(is.null(invx)) {
    data <- x$get()
    invx <- solve(data)
  }
  ## write inverse matrix into list x
  x$setinverse(invx)
  
  return(invx)
}

