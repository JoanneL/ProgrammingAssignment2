## This R code is to quickly restrieve an inverse of a matrix that is already 
## calculated and stored in the cache, when the matrix is remained unchanged.

## makeCacheMatrix is a list of functions that store a matrix and calculate
## the inverse assigned with the main function. The matrix and inverse can be
## retrieved by secondary functions get() and getinverse(). 

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
      x <<- y
      m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## The cacheSolve function looks at the matrix where makeCacheMatrix stores
## and verifies its inverse m that previously stored in getinverse function.
## It resturns a message and the cached m if it is not null. If not, it will
## store the data in makeCacheMatrix and calculates the inverse that to be
## returned and stored in makeCacheMatrix as well. 

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)){
        message("Getting Cached Data")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$getinverse(m)
      m
  ## Return a matrix that is the inverse of 'x'
}
