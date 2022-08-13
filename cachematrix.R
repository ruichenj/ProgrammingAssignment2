## I changed all "mean" functions to "inverse" for obtainig the inverse matrix.
##  

## In the first section of the code, I firstly
## set up a function to obtain numeric data.
## then, by setting m <- NULL, the data section
## is emptied for containing input data.
## A list is set at last to obatin matrix data.

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



## this section of code continues to obtain the inverse
## of a matrix. 


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
