# The function `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to
# 1.  set the matrix
# 2.  get the matrix
# 3.  set the inverse of the matrix
# 4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # initially set to NULL but it changes when you set the value
  inv <- NULL
  
  # set function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get matrix - sets the matrix but not the inverse
  get <- function() x
  
  # set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # get the inverse
  getinverse <- function() inv
  
  # introduce into a list
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setinverse`
# function.

cacheSolve <- function(x, ...) {
  
  # get of the inverse and check if it has been computed yet
  inv <- x$getinverse()
  
  # if has been computed before, return the computed inverse	
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if it has not been computed yet, get the matrix
  data <- x$get()
  
  # find the inverse
  inv <- solve(data, ...)
  
  # cache the result
  x$setinverse(inv)
  
  # return the result
  inv    
}
