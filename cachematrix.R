## Put comments here that give an overall description of what your
## functions do
###
# Functions to create a class that defines a list of methods specifically used 
# to cache the inverse of a matrix and calculate the inverse, when necessary. 
#
# makeCacheMatrix defines four methods to be used: set, get, setinverse, and getinverse.
# Define the data matrix using the set method. 
# i.e.: 
# > matData <- makeCacheMatrix()
# > matData$set(matrix(c(1,-1,1,2),2,2))
# > matData$get()
#      [,1] [,2]
# [,1]    1    1
# [,2]   -1    2
#
# cacheSolve then uses the object created using makeCacheMatrix to check if the 
# inverse has already been calculated and cached, in which case it return the 
# inverse, or calculates the inverse based on the data matrix stored.
# i.e.:
# > cacheSolve(matData)
#           [,1]       [,2]
# [1,] 0.6666667 -0.3333333
# [2,] 0.3333333  0.3333333
###

## Write a short comment describing this function
# Function to create a special "matrix" object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
     # initialize s
     s <- NULL
     # function to set the data matrix 
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     # function to recall data matrix, if it has been assigned, otherwise NA
     get <- function() x
     # function to store the inverse of the matrix
     setinverse <- function(inverse) s <<- inverse
     # function to recall the inverse of the matrix
     getinverse <- function() s
     # list with all the defined functions
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Write a short comment describing this function
# Function to compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     # recall cached inverse either inverse or NULL
     s <- x$getinverse()
     # if inverse has previously been cached, return the answer
     if(!is.null(s)) {
          message("getting cached data")
          return(s)
     }
     # if s = NULL, calculate the inverse of the matrix stored in x 
     # recall matrix stored in x
     data <- x$get()
     # Calculate inverse
     s <- solve(data, ...)
     # Cache (store) the matrix inverse to be called later
     x$setinverse(s)
     # return inverse matrix
     s
     
}
