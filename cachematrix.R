## A pair of functions to cache the inverse of a matrix

## The function makeCacheMatrix() caches the inverse of the matrix it consists of the
## get(), set(), getinverse() and setinverse() functions contained in a list.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set() <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <- inv
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## The function cacheSolve() finds the inverse of a matrix provided its not cached
## earlier, if the inverse has been calculated it is just retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

