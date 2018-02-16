## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function makeCacheMatrix uses a matrix as an input, set the value.
# get the value, set the inverse Matrix and get the inverse.
# Then the matrix can cache its own object

makeCacheMatrix <- function(x = matrix()) {
  invMatrix<-NULL
  setMatrix<-function(y){
    x<<-y
    invMatrix<<-NULL
  }
  getMatrix<-function()x
  setInverse<-function(inverse)invMatrix<<-inverse
  getInverse<-function()invMatrix
  list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)
}



## Write a short comment describing this function
## Function cacheSolve uses the output of makeCacheMatrix(matrix) as input and checks 
# whether inverse matrix from makeCacheMatrix(matrix) has been calculated already or not.
# To prevent from getting empty inverse matrix, the function gets the original matrix and uses
# solve function to set the invertible matrix
# And "Getting Cached Invertible Matrix" will reveal that the inverse matrix has already value in it.
cacheSolve <- function(x, ...) {
  invMatrix<-x$getInverse()
  if(!is.null(invMatrix)){
    message("Getting Cached Invertible Matrix")
    return(invMatrix)
  }
  MatrixData<-x$getMatrix()
  invMatrix<-solve(MatrixData,...)
  x$setInverse(invMatrix)
  return(invMatrix)
        ## Return a matrix that is the inverse of 'x'
}

