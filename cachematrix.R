## This module contains 3 functions: 
# makeCacheMatrix: Creates a specialMatrix as list and is able to cache the inverse
# cacheSolve:      This function returns the inverse if possible from cache
#                  if no cache exists it calculates the inverse and stores it in
#                  the special matrix object
# testMatrix:      function which can be called without any parameters to do some
#                  unit testing.

## function:  makeCacheMatrix()
## parameter: Matrix being a standard R matrix, default is an empty Matrix
## return:    a list with 4 entries.
makeCacheMatrix <- function(Matrix = matrix()) {
  # flush cache
  cachedInv <- NULL
  
  # function to set a new matrix and flush the cached inverse matrix
  set <- function(newMatrix) {
    Matrix     <<- newMatrix
    cachedInv  <<- NULL
  }
  
  # function to get the current stored Matrix
  get    <- function() {
    Matrix
  }
  
  # function to cache the inverse Matrix which is the input parameter
  # cached is pushed up 1 environment 
  setinv <- function(invMatrix) {
    cachedInv <<- invMatrix
  }
  
  # function to get the current cached Inverse Matrix
  getinv <- function() {
    cachedInv
  }
  
  # return a list wih 4 elements
  return(
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv
    )
  )
}


## function:  cacheSolve()
## parameter: specMatrix being a 'special matrix', actually a list
##            created by the makecacheMatrix function 
## return:    inverse matrix, if cached is used a console message is given.
cacheSolve <- function(specMatrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  # get the current cached inverse Matrix
  invMatrix <- specMatrix$getinv()
  
  # if a cached Matrix exist print a message that the cache is used
  # if not than calculate the inverse and cache the inverse in the 
  # special matrix
  
  if(!is.null(invMatrix)) {
    message("getting cached data")
  } else {
    # get matrix data
    Matrix    <- specMatrix$get()
    
    # calculate the inverse
    invMatrix <- solve(Matrix, ...)
    
    # cache the inverse in the special matrix
    specMatrix$setinv(invMatrix)
  }
  
  # return the inverse Matrix
  return(invMatrix)
}


# Test
testMatrix <- function() {
  # Create a special matrix object given a matrix as parameter
  M <- makeCacheMatrix(matrix(data = c(0,2,2,0), nrow = 2, ncol = 2))

  # calculate inverse without a cache
  print("")
  print("This should be without cache")
  print(cacheSolve(M))
  
  # inverse from the cache
  print("")
  print("This should be with cache")
  print(cacheSolve(M))
  
  # Change the special matrix object, cached should be flushed  
  M$set(matrix(data = c(0,3,3,0), nrow = 2, ncol = 2))
  # calculate inverse without a cache
  print("")
  print("This should be without cache")
  print(cacheSolve(M))
  # inverse from the cache  
  print("")
  print("This should be with cache")
  print(cacheSolve(M))
  
  # What happens if no matrix is provided
  M <- makeCacheMatrix()
  print(cacheSolve(M))
  print(cacheSolve(M))
}