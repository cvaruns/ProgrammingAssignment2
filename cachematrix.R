## The purpose of these functions is to create a cache matrix, store it and return the
## inverse of the cached matrix stored.

## makeCacheMatrix is the function used to take a 'square matrix' as an argument and create a cache matrix

makeCacheMatrix <- function(x = matrix())  ##Accepts only Square Matrix
  { 
  i <- NULL
  y <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() ##Stores the matrix in cache
  {
    x
  }
  setinverse <- function(inv){ i <<- inv}
  getinverse <- function(){ i}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve is the function that generates and returns the inverse of the cache matrix.

cacheSolve <- function(x, ...) ## Return a matrix that is the inverse of 'x'
{
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...) ##Obtaining Inverse of Cached Matrix
  x$setinverse(i)
  i
}
##This function only works if the Determinant of the cached matrix is something other than 0.
##Else it will generate an error.
