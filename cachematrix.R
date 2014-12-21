## == chachematrix.R ==
## -- By: David Pitt (github.com/dpitty)

## makeCacheMatrix() is a custom implementation of the matrix class
## which allows chaching the matrix's inverse (aka "solve()") after one
## computation, such that future calls to cacheSolve() retrieve the
## inverse value instantly, instead of requiring costly repeated
## calls to solve().
## NOTE: the program only works on invertible matrices and will throw an
## error on cacheSolve() if the input matrix is not invertible.

## Function makeCacheMatrix(x = matrix()) converts a standard matrix 
## into a cahce-able one, represented as a list of functions which are
## used in cacheSolve() (not intended for the programmer's direct use).
## -- Usage: --
## myNormalMatrix <- matrix(nrow = 2, ncol = 2, 1:4)
## myCacheMatrix <- makeCacheMatrix(myNormalMatrix)

makeCacheMatrix <- function(x = matrix()) {
  savedInverse <- NULL
  setmatrix <- function(y) {
    x <<- y
    savedInverse <<- NULL
  }
  getmatrix <- function() { 
    x
  }
  setinverse <- function(initialInverse) {
    savedInverse <<- initialInverse
  }
  getinverse <- function() {
    savedInverse
  }
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function cacheSolve(x, ...) works the same as solve() on an ordinary
## matrix, but requires a chacheMatrix as its input. Returns the inverse
## of the original matrix.
## -- Usage: --
## inverse <- cacheSolve(myCacheMatrix)
## cacheSolve() allows passing multiple parameters with the same rules
## as the solve() function.

cacheSolve <- function(x, ...) {
  cachedInverse <- x$getinverse()
  if (!is.null(cachedInverse)) {
    message("getting cached inverse")
    return(cachedInverse)
  } else {
    originalMatrix <- x$getmatrix()
    inverse <- solve(originalMatrix, ...)
    x$setinverse(inverse)
    inverse
  }
}
