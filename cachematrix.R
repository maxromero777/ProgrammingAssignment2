## Calculates inverse of a matrix (retrieved from cache if it has already been calculated)

## Create Matrix object.

makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(inv) {
    matrix <<- inv
  }
  getinverse <- function() {
    matrix
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse of a matrix returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
  matrix <- x$getinverse()
  if(!is.null(matrix)) {
    message("cached matrix")
    return(matrix)
  }
  m <- x$get()
  matrix <- solve(m) %*% m
  x$setinverse(matrix)
  matrix     
}


## For Testing use
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1

matrix<-makeCacheMatrix(m1)
matrix
cacheSolve(matrix)
cacheSolve(matrix)
