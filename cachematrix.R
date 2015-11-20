## This function calculate inverse of an invertible matrix
## and cache it to avoid potential time-consuing
## inversion.

## makeCachematrix creates a special matrix that
## actually is a list of length 4. 

makeCacheMatrix <- function(x = matrix()) {
  # set the value of the matrix
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  # get the value of the matrix 
  get <- function() x
  # set the value of its inversion
  setsolve <- function(solve) m <<- solve
  # get the value of its inversion
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## calculate inversion of the special matrix made
## via above function. However, if its inversion
## exists in the cache. Message will be shown and
## cached value be returned.
## Example:
## test <- matrix(c(2,2,3,2),nrow=2)
## v <- makeCacheMatrix(test)
## cacheSolve(v) # first time call 
# you will see it returns its inversion.

## cacheSolve(v) # second time call
# the message "getting cached data" will be shown in the console.
# and the cached inversion is also displayed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)){
    message('getting cached data')
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
