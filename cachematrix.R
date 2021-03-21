## Submission for Week 3 of R Programming 
## Forgot to stage befor committing and pushing

## makeCacheMatrix() has a list of functions to modify a matrix and stores
## the result of the modified matrix into the cache in order to reduce
## processing overhead when calling the function repeatedly.  This is provided
## no change has been made to the original matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve is used to get the inverse of a matrix

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
