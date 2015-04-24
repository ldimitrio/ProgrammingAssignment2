## The cachematrix.R file creates a structure in which the value of
## a matrix and of its inverse can be stored. It also creates a function 
## that calculates the inverse of a matrix, if it has not been calculated yet.

## The first function, makeCacheMatrix creates 
## a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x= matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)  
}


## The cacheSolve function returns the inverse of a given matrix:
## firstly, it verifies if the inverse has already been calculated. 
## If so, it returs the value cached in x$getinv
## If not, it calculates the inverse using the "solve" function. 
## It takes as an argument a "matrix" that has the structure given in makeCacheMatrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      message("solving inverse matrix")
      inv <- solve(data, ...)
      x$setinv(inv) #update the value in the cache for the next call
      inv
}
