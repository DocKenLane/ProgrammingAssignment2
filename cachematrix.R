## Assignment 2 for R programming class
## 
## Modified example names to fit context

makeCacheMatrix <- function(x = matrix()) {
  Ainv <- NULL
  set <- function(y) {
    x <<- y 
    Ainv <<- matrix()
  }
  get <- function() x
  setinv <- function(inv) Ainv <<- inv
  getinv <- function() Ainv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Ainv <- x$getinv()
  if(!is.null(Ainv)) {
    message("getting cached data")
    return(Ainv)
  }
  data <- x$get()
  Ainv <- solve(data, diag(nrow(data)))
  x$setinv(Ainv)
  Ainv
}