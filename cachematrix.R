# Define function to set the value of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  # It clears the old inverse from the cache
  set <- function(y) {
    x <<- y # Set the value
    inv <<- NULL # clearing the cache value 
  }
  get <- function() x # function  to get the value of the matrix 
  setinverse <- function(inverse) inv <<- inverse 
  # Function for the  inverse. This is used when nothing in cached 
  getinverse <- function() inv
  # Define function to get the inverse
  # returns a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# returns inversve of the matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse() # gets the cahced value of the inverse
  if(!is.null(inv)) { # if chaced value is not empty then it can be returned
    message("getting cached data")
    return(inv)
  }
  data <- x$get() # get value of matrix
  inv <- solve(data) # calculate  the ivnerse
  x$setinverse(inv) # cache the result
  inv # return the value
}
