## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## print(x)
  
  invMx <- NULL
  
  set <- function(y) {
    x <<- y
    invMx <<- NULL
  }
  
  get <- function() x
  
  setinverse<- function(inverse) invMx <<- inverse
  
  getinverse <- function() invMx
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invMx <- x$getinverse()
  
  if (!is.null(invMx)) 
  {
    message("getting inverse matrix from cache")
    
    return(invMx)
  } 
  else 
  {
    invMx <- solve(x$get())
    x$setinverse(invMx)
    
    return(invMx)
  }
