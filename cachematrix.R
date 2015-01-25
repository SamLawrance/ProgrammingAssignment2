## Functions for building a cacheable matrix object, and solving the matrix
## inverse (using cached results after initial calculation).
##
## Example usage (create matrix, retrieve it, solve inverse, solve again
## for cached result)
##
# > mat <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# > mat$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(mat)
# [1] "Using calculated result"
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(mat)
# [1] "Using cached result"
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## makeCacheMatrix takes 1 argument (a matrix) and constructs a list with the
## following functions:
## $get() - retrieve the matrix
## $set(y) - set the matrix to y
## $getminv() - get the matrix inverse
## $setminv(minv) - set the matrix inverse to minv
## No calculations are performed.  The functions above simply set and retrieve
## matrix values. If no matrix is specified in the call to makeCacheMatrix, a
## 1x1 matrix containing NA is used.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setminv <- function(minv) m <<- minv
  getminv <- function() m
  list(set = set, get = get, setminv = setminv, getminv = getminv)
}

## cacheSolve accepts a list (created by makeCacheMatrix) and returns the
## inverse of the matrix.  The inverse is calculated once only and then cached;
## subsequent calls return the cached value.
#
## For markers: the structure of this function is slightly different to the
## example, however the result is the same.  Ordinarily I would not leave
## the printf statements in, however they are useful to demonstrate that
## the function does what it is supposed to.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getminv()
  if (is.null(m)) # is there a cached matrix inverse?
  {
    print("Using calculated result") # no, calculate inverse
    data <- x$get()
    m <- solve(data)
    x$setminv(m)
  } else print("Using cached result")
  m
}
