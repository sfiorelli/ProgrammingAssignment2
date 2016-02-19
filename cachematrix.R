
## makeCacheMatrix and cacheSolve functions allow for calculating and
## caching of of a matrix inverse.
##
## Example use:
##
##> z <- matrix(c(1,-1,1,1),nrow=2)
##> x <- makeCacheMatrix(z)
##> cacheSolve(x)
##[,1] [,2]
##[1,]  0.5 -0.5
##[2,]  0.5  0.5

## makeCacheMatrix stores functions and inverves for retreval

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve uses functions from object created in makeCacheMatirx to return inverse

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  # Return if cached inverse exists
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  # If no cached value, try to calculate inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
