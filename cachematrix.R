## This is a combination of 2 functions, one which caches an inverse 
## matrix and the other either surfaces the inverse if cached or computes it
## if it isn't cached

## This library has the function "ginv" which calculates the inverse of a matrix
## even if the matrix is not square
library(MASS)

## This function caches results of the calculation of a matrix


makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y){
    x <<- y
    minv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) minv <<- inverse
  getinv <- function() minv
       list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


## This function calculates get the cached inverse function of a matrix 
## or calculates it if it's not cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
       if(!is.null(minv)) {
           message("getting cached data")
           return(minv)
       }
       data <- x$get()
       minv <- ginv(data)
       x$setinv(minv)
       minv
}
