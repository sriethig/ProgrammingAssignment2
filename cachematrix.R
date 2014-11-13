## matrix invasion is a costly operation, so if the inversion
## of the special matrix in makeCacheMatrix has already been 
## calculated, the value is stored in the local variable
## cacheMatrix and therefore doesn't have to be calculated again


## makeCacheMatrix builds the cacheMatrix, where the value
## of the inversion is stored

makeCacheMatrix <- function(x = matrix()) {
    # initialize the variable cacheMatrix
    cacheMatrix <- NULL
    
    # set function <- sets the matrix x
    set <- function(y) {  
      x <<- y
      cacheMatrix <<- NULL
    }
    
    # get function -> returns the matrix x
    get <- function(){
      x
    } 
    
    # set the inverse of the matrix x
    setinverse <- function(inverse){
      cacheMatrix <<- inverse
    }
    
    # get the inverse of the matrix x
    getinverse <- function(){
      cacheMatrix
    }
    
    # list of available functions
    list(get = get,
         set = set,
         getinverse = getinverse,
         setinverse = setinverse)
}


## cacheSolve takes a matrix and calculates its inverse
## the result is cached locally in the variable cacheMatrix

cacheSolve <- function(x, ...) {
  # store result of x$getinverse in cacheMatrix
  # see makeCacheMatrix function to see, what happens there
  cacheMatrix <- x$getinverse()
  
  # if the local variable cacheMatrix stores a value
  # return the stored value
  if(!is.null(cacheMatrix)) {
    message("getting cached data")
    return(cacheMatrix)
  }
  
  # get the matrix from makeCacheMatrix and give it to data
  data <- x$get()
  cacheMatrix <- solve(data) # here the inversion really happens
  x$setinverse(cacheMatrix) # set the value 
  cacheMatrix # return value
}
