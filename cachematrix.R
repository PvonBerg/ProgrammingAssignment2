.
makeCacheMatrix <- function(x = matrix()) {
  #This function creates a special "matrix" object that can cache its inverse
  m <- NULL ## define the cache m
  set <- function(y) {
    x <<- y ## assign the input matrix y to x
    m <<- NULL ## re-initialize m to null
  }
  get <- function() x ## return x
  setinverse <- function(inverse) m <<- inverse ## set the cache m to the inverse of the matrix x
  getinverse <- function() m ## return the inverse of x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
  
  m <- x$getinverse()#get the inverse of x
  if(!is.null(m)) {#if m is not empty return m
    message("getting cached data")
    return(m)
  }
  data <- x$get()#compute the inverse of x, save it in the cache and print it
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
