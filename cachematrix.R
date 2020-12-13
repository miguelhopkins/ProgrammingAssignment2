## Put comments here that give an overall description of what your
## functions do
## "makeCahceMatrix" creates a Matrix that can be cached
## and used as an argument in "cacheSolve".
## "cacheSolve" will take a matrix and calculate its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  IM <- NULL 
  ## "IM" stands for "Inverse Matrix".
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  ## The "set" function only sets the value of "x" in another environment.
  get <- function() {x}
  ## "Get" takes the value of "x"
  setinverse <- function(inverse) {IM <<- inverse}
  ## Sets the new value of "IM"
  getinverse <- function() {IM}
  ## Takes the value of "IM"
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## This list allows to later retrieve the functions inside.


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## This function only works with matrices created
  ## using the "makeCacheMatrix"
  IM <- x$getinverse()
  if(!is.null(IM)){
    message("getting cached data")
    return(IM)
  }
  ## If "IM" has been calculated before, it will retrieve it
  ## If "IM" is NULL, it will calculate it.
  data <- x$get()
  IM <- solve(data, ...)
  x$setinverse(IM)
  IM
}