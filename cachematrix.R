## The makeCacheMatrix function returns a list of functions 
## to access a matrix 'x' and its inverse 'i'
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  ## this function assigns the matrix 'y'
  ## the inverse matrix 'i' is reset
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  
  getInverse <- function () i
  
  return (list(set=set, get=get, setInverse=setInverse, getInverse=getInverse))

}

## Return a matrix that is the inverse of the matrix 'x' utilizing the "cache"
cacheSolve <- function(x, ...) {
  
  inverse <- x$getInverse()
  
  ## check if the inverse is set, if not 
  ## compute the inverse and set it into the "cache"
  if (is.null(inverse)) {
    
    inverse <- solve(x$get(), ...)  
    x$setInverse(inverse)
    
  } else {
  ## utilize the "cache" and return the inverse from value "cached" in x  
    print("getting cached data")
  }
  
  return (inverse)
}
