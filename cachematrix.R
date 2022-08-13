## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL                ##inverse set to NULL in the beginning
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {               ##To check if inverse if null
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

##Below is to check whether the code does its work!
my_matrix <- matrix(c(1,3,5,7),2,2)
solution <- makeCacheMatrix(my_matrix)
cacheSolve(solution)