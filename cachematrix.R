 Put comments here that give an overall description of what your
 functions do

##  The 2 functions need to run in sequence.  makeCacheMatrix function is a help
##  for the cacheSolve function.  As with makeVector you send in a vector as
##  parameter x it is prepared for processing by the cacheSolve function.
##  The cacheSolve function returns an inverse or the cached inverse matrix with
##  a message stating so.
  
## Tested by using this constructed matrix for x
## v1 <- c(1, 3)
## v2 <- c(2, 4)
## matrix <- rbind(v1, v2)
## matrix

## Write a short comment describing this function
## Run this function first to prepare the matrix for processing
## A special matrix results (2x2)
makeCacheMatrix <- function(x = matrix()) {
  ## matrix is empty
  matrx <- NULL
  
  ## 1) set the value of the matrix
  set <- function(y) {
    x <<- y 
    matrx <<-NULL
  }
  
  ## 2) get the value of the matrix
  get <- function() x
  
  ## 3) set the value of the inverse
  setInverse <- function(solve)matrx <<- solve
  
  ## 4) get the value of the inverse
  getInverse <- function() matrx
  
  ## return a list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  

}


## Write a short comment describing this function
## On the first pass this function will take the special matrix 
## and make an inverse matrix
## On the second pass of the same matrix the message is returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## check if inverse is already calculated
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## calculate the inverse and return it as inverse
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
  
}
