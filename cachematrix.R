## Caching the Inverse of a Matrix

## Creates a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x   ##get matrix
  setinverse <- function(inverse) m <<- invert   ##set inverse matrix
  getinverse <- function() m   ##get inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Calculates the Inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
  
  
}
