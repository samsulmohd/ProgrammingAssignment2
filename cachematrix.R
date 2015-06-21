## Caching the Inverse of a Matrix

## 'makeCacheMatrix' function creates a special "matrix" object 
##  that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
  ##initialize m to NULL
  m <- NULL
  
  ##matrix argument passed to makeCacheMatrix with 
  ## x and m as value to new environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
    
  get <- function() x                         ##get matrix
  setinverse <- function(solve) m <<- solve   ##set inverse matrix
  getinverse <- function() m                  ##get inverse matrix
  list(set = set, get = get,                  ##list all the values in 
       setinverse = setinverse,               ## makeCacheMatrix
       getinverse = getinverse)
  
}


## 'cacheSolve' function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix. If the inverse has already been calculated 
##  (and the matrix has not changed), then the cachesolve should retrieve 
##  the inverse from the cache.


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  ##getting from cached if exist
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##calculate the inverse if not cached
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
    
}
