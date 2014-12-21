## Functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {m<-NULL ## sets m to NULL
                                           setMatrix<-function (y){
                                             x<<-y
                                             m<<-NULL ## re-initializes m in the parent environment to Null}
                                           getMatrix<-function () x ##returns to stored matrix
                                           setMatrixinverse<-function (inverse) m<<-inverse ## sets the cache m equal to the inverse of the matrix x
                                           getMatrixinverse<-function () m ## returns the cached inverse of x
                                           ## returns a list of functions
                                           list(setMatrix=setMatrix, getMatrix=getMatrix, setMatrixinverse=getMatrixinverse)}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## returns a matrix that is the inverse of 'x'
  m <- x$getMatrixinverse()
  ## if m value exists return it
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  # otherwise gets the matrix and calculates the inverse and store it in m
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setMatrixinverse(m)
  m
}
