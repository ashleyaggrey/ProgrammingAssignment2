## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#set and get the value of the matrix and of the inverse function

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
 
  
  get <- function() x
  set_inverse <- function(solve) i <<- solve
  get_inverse <- function() i
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}



## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
#calculate the inverse of the matrix. If it has already been calculated, then calculation 
# will be skipped and inverse will be obtained from the cache via the set_inverse function



  cacheSolve <- function(x, ...) {
    
    i <- x$get_inverse()
    
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$set_inverse(i)
    i
  
  }
