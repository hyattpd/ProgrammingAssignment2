## Second Programming Assignment

## Caches the inverse of a matrix to avoid having
## to compute it multiple times.
##
## Example:
##     m <- cbind(c(7,3), c(-2,5))
##     m1 <- makeCacheMatrix(m1)
##     inverse <- cacheSolve(m1)

## This function returns a list of functions that
## get or set the matrix, and get or set the
## inverse of the matrix.  This function must be
## called first on a matrix in order to convert it
## to a list that can be passed to 'cacheSolve'.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    #  <<- used for different environment
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invmat <<- inverse
  getinverse <- function() invmat
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function checks to see if the inverse
## is in the cache.  If it is, we use it.  If not,
## we calculate the inverse.  Reads in an object
## supplied by makeCacheMatrix.  

## The function assumes the user will call the set
## function from makeCacheMatrix again if the
## matrix has changed, so we don't check to see if
## it's changed in this function (which would take
## too long and defeat the point of caching).

cacheSolve <- function(x, ...) {
  invmat <- x$getinverse()
  # if value is in the cache, use it
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  # else solve for the inverse
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinverse(invmat)
  invmat
}
