## makeCacheMatrix creates an object called matrix together with 
## its inverse (set to NULL initially) and four sibbling functions set, get, 
## setinverse and getinverse. The code is taken from the assigment example and 
#  modified for matrices.
# Syntax: myMatrix=makeCacheMatrix(matrix(1:4,2,2))

makeCacheMatrix <- function(x = matrix()) 
  {
  x_inv <- NULL
  set <- function(y) 
    {
    x <<- y
    x_inv <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) x_inv <<- inverse
  getinverse <- function() x_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

  }


## cacheSolve computes the inverse of the makeCacheMatrix object x.

cacheSolve <- function(x, ...) 
  {
        
  x_inv <- x$getinverse()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinverse(x_inv)
  x_inv
  
}
