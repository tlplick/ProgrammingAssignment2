## These functions demonstrate how to scope variables and use
## variables from a different scope using the <<- operator

## the makeCacheMatrix function creates a list of functions that
## manage an original matrix and then the inverted version of that matrix

## the cacheSolve function takes a matrix, looks to see if it has
## already been inverted (via the makeCacheMatrix) and if so, use the
## cached value and if not, invert the matrix and then store the inverted
## matrix in the cache.  

#This function creates a special "matrix" object that can cache its inverse  

makeCacheMatrix <- function(matrixToInvert = matrix()) {
  invertedMatrix <- NULL
  
  #inner function definition to set the cached matrix to be inverted
  set <- function(y) {
    matrixToInvert <<- y
    invertedMatrix <<- NULL
  }
  
  #inner function definition to get the cached matrix to be inverted
  get <- function() matrixToInvert
  
  #inner function definition to set the cached inverted matrix
  setInvertedMatrix <- function(ivMatrix) invertedMatrix <<- ivMatrix
  
  #inner function definition to get the cached inverted matrix
  getInvertedMatrix <- function() invertedMatrix
  
  #creating the list to return contianing all of the cache methods
  #allowing access to the cached matrixes  
  list(setOriginalMatrix = set, getOriginalMatrix = get,
       setInvertedMatrix = setInvertedMatrix,
       getInvertedMatrix = getInvertedMatrix)
}


## Return a matrix that is the inverse of 'cacheMatrix'

cacheSolve <- function(cacheMatrix, ...) {
  
  #first, look to see if the inverted matrix has already been
  #calculated and set in the cache
  invertedMatrix <- cacheMatrix$getInvertedMatrix()
  if(!is.null(invertedMatrix)) {
    message("getting cached inverted matrix!")
    return(invertedMatrix)
  }
  
  #nope, get the original matrix
  matrixToInvert <- cacheMatrix$getOriginalMatrix()
  
  #Invert it
  invertedMatrix <- solve(matrixToInvert)
  
  #and then cache it!
  cacheMatrix$setInvertedMatrix(invertedMatrix)
  
  # now return the newly inverted matrix.  
  return(invertedMatrix)
}
