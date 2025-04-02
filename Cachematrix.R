## makeCacheMatrix creates a special matrix that holds functions to:

#Set the matrix value

#Get the matrix value

#Set the cached inverse

#Get the cached inverse



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  # the cashee for inverse
  
  
  # function for matrix value
  set <- function(y) {
    x <<- y
    m <<- NULL  #reset when the matrix is changed
  }
  
  # funtion to get matrixvalue
  get <- function() x
  
  # funtion set inverse
  setinverse <- function(inverse) m <<- inverse
  
  # function to get the cached inverse
  getinverse <- function() m
  
  # return list 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve checks if the matrix inverse is already cached. If it's cached, it just returns it. 
## If not, it calculates the inverse using solve(), stores it in the cache, and then returns the result.

cacheSolve <- function(x, ...) {
  # retrieve  cached inverse
  m <- x$getinverse()
  
        # if exists return it
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # otherwise compute the inverse and cache it
  data <- x$get()  
  m <- solve(data, ...)  
  x$setinverse(m)     # cache the inverse
  m  # return the inverse
}

## end
