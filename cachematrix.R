## Put comments here that give an overall description of what your
## functions do

##The following functions allow the extraction of the inverse 
## matrix of a given invertible matrix in case the inverse of the matrix
## was already calculated and kept in memory.
##Otherwise, the inverse is calculated and kept in cache
#thus avoiding costly calculations.



## Write a short comment describing this function

## makeCacheMatrix creates a list of 4 methods to be used with
## a given matrix m. The 4 methods are:
##1.set - Set the value of the matrix
##2.get - Get the value of the matrix
##3.setinv - Set the value of the inverse of the matrix
##4.getinv - Get the value of the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Write a short comment describing this function
##cacheSolve verifies if the inverse of the matrix
## is already calculated and recalls it from memory
##if it is. Otherwise, it calculates the inverse,
##and saves it to memory.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- m$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setinv(inv)
  inv
}
