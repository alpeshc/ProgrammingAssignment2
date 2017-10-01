## Performing the inverse of a function is costly in terms of CPU.  The R programme is used to demonstrate
## the benefit of caching results.  If the inverse of the matrix has already been calculated, then
## the cached value should be used, which means we do not need to perform the costly calculation.
## If the inverse of the matrix is not already cached then it will be calculated and then stored
## in cache using the '<<' caching operator.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It performs the following steps:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

 makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inverse) m <<- inverse
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix) 
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has
## not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached value for inverse of matrix")
    return(m)
  }
  data <- x$get()
  message("We don't have a cached value, so we have to calculate inverse of matrix...")
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
  }

## Test cases...

## create a matrix to use
##x <-makeCacheMatrix(matrix(c(4,2,7,6),nrow=2,ncol=2))
## Get the matrix
##x$get()
## Check if we have a cached value...we shouldn't have at this point.
##x$getinversematrix()
## now solve the matrix...we don't have a cached value so the inverse calculation is performed
##cacheSolve(x)
## this time we should get the cached value, saving time on the calculation.
##cacheSolve(x)

