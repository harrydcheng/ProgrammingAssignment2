## Harry Cheng - Coursera R Programming Week 3 Assignment

## makeCacheMatrix is analogous to the makeVector example, thus creating a "special matrix"
## which is a list with setting and getting functionality, and the ability to store its 
## own inverse in cache. The key data objects are:
## m: the argument matrix
## invm: the inverse of m



makeCacheMatrix <- function(m = matrix()) {

  invm <- NULL
  
  #set function: m gets y, invm gets wiped
  set <- function(y) {
    m <<- y
    invm <<- NULL
  }
  
  #get function: returns m
  get <- function() m
  
  #setinv function: invm gets newinv
  setinv <- function(newinv) invm <<- newinv 
  
  #getinv function: returns invm  
  getinv <- function() invm
  
  #creates list of named functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve is analogous to the cachemean example. It computes the inverse of a matrix
## created using makeCacheMatrix if it has not already been solved, and stores that in cache.
## Subsequent calls of cacheSolve get the inverse from cache if the matrix has not changed.

cacheSolve <- function(x, ...) {
        
  invm <- x$getinv()
  if(!is.null(invm)) {
    message("getting cached inverse")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinv(invm)
  message("inverse not cached, solving from scratch")
  invm
}


## Matrices for testing
# matrix(rexp(4), 2) make a random square matrix
# matrix(c(1,2,3,4), 2, 2) square matrix with 1234 as entries
