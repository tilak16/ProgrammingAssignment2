## Code written by Vinayak Tilak - December 16, 2014
## We are creating two functions makeCacheMatrix to create a cache matrix 
## and its inverse matrix
## The second function decides to either invert a matrix or not depending on
## whether there is a NULL matrix in the Cache.

## Creates for a list of four functions used to set, get matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
m<-NULL #We start with m as NULL
#This below function is to assign the matrix, not used by cacheSolve.
#Used as good programming hygine
setmatrix <- function(y){
  x<<-y
  m<<-NULL
}
#The below function is to get the matrix to be inverted
getmatrix<-function() x
#The below matrix is to set the inverse of the matrix to m.
#Doing this with the <<- function is to ensure we look in the parent environment
#for solve
setSolve <- function(solve) m<<-solve
#The below function returns the value of m (Inverse of matrix) in the cache
getSolve <- function() m
list(setmatrix = setmatrix, getmatrix = getmatrix,
     setSolve = setSolve,
     getSolve = getSolve)
}


## This function checks if inverse has been calculated for the same matrix
##if so returns the value in the cache, if not calculates the inverse of the
##matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getSolve() ##gets the cache
  if(!is.null(m)) {##Checking if cache has the inverse
    message("getting cached matrix")
    return(m)
  }
  data <- x$getmatrix()#getting the original matrix
  m<-solve(data,...)#Calculating the inverse of the matrix
  x$setSolve(m)##Filling the cache with the inverse of the matrix
  m
}
