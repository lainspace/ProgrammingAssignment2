###############################################
#R-programming Assignment 2 - Lexical Scoping
#caching inverse of a matrix
###############################################

#clear the memory first
rm(list = ls())

#-----------------------------------------------
#Creating a list containing a function to:
#SEt the value of a matrix
#get the value of a matrix
#set the value of the inverse of the matrix
#get the value of the inverse of the matrix
#-----------------------------------------------
makeCacheMatrix <- function( x = matrix()){
  #sanity check
  if (anyNA(x)) {
    message("missing elements in matrix, wrong")
    break
  }
  else if (dim(x)[1] != dim(x)[2]) {
    message("Not a square matrix, wrong")
    break
  }
  
  #inverse of the matrix
  invx <- NULL
  
  #set the value of the matrix
  set <- function() {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(inversex) invx <<- inversex
  getinv <- function() invx
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#-----------------------------------------------
#Get the inverse of a matrix
#if already calculated get the cached data
#-----------------------------------------------

cacheSolve <- function(x, ...) {
  invx <- x$getinv()
  if (!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data)
  x$setinv(invx)
  invx
}

#-----------------------------------------------
#Try use the invInverse and cacheInverse functions
#-----------------------------------------------
x <- matrix(c(1,3,2,7,9,8,2,4,1),nrow = 3, ncol = 3)
invfunc <- makeCacheMatrix(x)
x_inverse <- cacheSolve(invfunc)
print(x_inverse)

#IF getting the inverse again
x_inverse2 <- cacheSolve(invfunc)
print(x_inverse2)

