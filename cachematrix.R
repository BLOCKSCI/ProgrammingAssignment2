## R Programming: Assignment #2. Created 10/21/2015

## Goal: Create 2 functions that cache the inverse of a matrix and 
## then decide if I need to compute the inverse or pull it from the cache 


## Test Matrix if you want to run this:
## x <- matrix( c( 2, 3, 4, 5, 6, 7, 8, 9, 10), nrow=3, ncol=3)


## Using the example for this assignment as a template, 
## we can modify the code so it operates on a matrix instead of a vector. 
## Note we use: Solve insead of mean. Matrix instead of vector. 


## calculates the inverse of a Matrix and send it to the acche
makeCacheMatrix <- function(x=matrix()) {  ##Default set to empty matrix. 
  
  m <- NULL
  set <- function(y) {
    x <<- y  ## search the parent environment for x
    m <<- NULL  ## search parent environment for m
  }
  get <- function() x
  setMyMatrix <- function(solve) m <<- solve ##solve takes the inverse. 
  getMyMatrix <- function() m
  list(set = set, get = get,  ## Set then get value of matrix and it's inverse. 
       setMyMatrix=setMyMatrix,
       getMyMatrix=getMyMatrix)
  
}

## Checks to see if the inverse is in the Cache and returns either the cached
## inverse or calculates and returns the inverse. 

cacheSolve <- function(x, ...) {
  
  m <- x$getMyMatrix() 
  ##Check to see if the inverse has been calculated already
  if(!is.null(m)) {
    message("Retrieving from Cache")
    return(m) ## Pulls the value from the Cache
  }
  MyMatrix <- x$get()  ##search by name for x
  m <- solve(MyMatrix, ...) ##Solve for the inverse if needed
  x$setMyMatrix(m)
  m
}

