## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      get <- function()x
      getinv <- function()inv
      set <- function(y){
        x<<- y
        inv <<- NULL
      }
      setinv <- function(inverse1) inv <<- inverse1
      list(get = get, set = set, getinv = getinv, setinv = setinv)
}


## Check if inverse already exists. If not, calculate inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
  
}
