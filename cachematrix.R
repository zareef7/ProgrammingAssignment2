## Put comments here that give an overall description of what your
## functions do
## To write a pair of functions "makeCacheMatrix" and "cacheSolve" that cache inverse of a matrix

## Write a short comment describing this function
## a function which creates special "matrix" object that can cache inverse for the input which is an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function
## a function computing the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse is calculated 
## (plus the matrix not changed), cachesolve  retrieves inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}

