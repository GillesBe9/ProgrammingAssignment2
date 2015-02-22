## Put comments here that give an overall description of what your
## functions do

# set function assigns the argument to x
# get function returns the matrix
# Returns the Inverse
makeCacheMatrix <- function(x = matrix()) {
  Inv_x <- NULL
  set <- function(y){
    x <<- y
    Inv_x <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) Inv_x <<- solve
  getInverse <- function() Inv_x
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}



## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.

# Retrives the most recent value for the inverse
# If the value of Inverse is NOT null, cacheSolve returns that value
# If the value of Inverse is NULL, then you retrive matrix x
cacheSolve <- function(x, ...) {
  Inv_x <- x$getInverse()
  if(!is.null(Inv_x)){
    message("getting cached inverse matrix")
    return(Inv_x)
  } else {
    Inv_x <- solve(x$get())
    x$setinverse(Inv_x)
    return(Inv_x)
} 
} 
