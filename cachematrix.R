## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the reverse of the matrix by using solve function
  setsolve <- function(solve) m <<- solve
  ## get the reverse of the matrix
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Get the reverse of the matrix if it has already been calculated
  m <- x$getsolve()
  ## If m is not null then return the reverse of the matrix stored in the cache
  if(!is.null(m)) {
    message("getting cached matrix reverse")
    return(m)
  }
  ## If m is null then we compute the reverse of the matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}