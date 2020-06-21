## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#the makeCacheMatrix takes a matrix as its argument 
#it creates an R object and stores a matrix and its inverse in it
#makeCacheMatrix creates its own environment within the global environment.
#its environment consists of functions set() get() setinv() getinv() and variables x and inv
#set() assigns the input value to x and NULL value to inv 
#inv is defined in the parent environment,in setinv() using the '<<-' operator a value is assigned to it
#getinv() is used to retrieve the value of inv from the parent environment
#finally all the functions are assigned values and stored in a list to be returned to the parent environment

makeCacheMatrix <- function(x = matrix()) {
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

## Write a short comment describing this function
#cacheSolve requires an object returned by makeCacheMatrix() in order to retrive the inverse from the cached value
#first the inverse is retrieved using x$getinv
#if the its value is not NULL the cached inverse is returned to the parent environment
#if it is NULL, the input matrix is obtained and the inverse is calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
