#https://class.coursera.org/rprog-033
#Programming Assignment 2

#In this example we introduce the <<- operator which can be used to assign a value
#to an object in an environment that is different from the current environment. 
#Below are two functions that are used to create a special object
#that stores a numeric vector and cache's its mean.

#####
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#The first function, makeCacheMatrix creates a special "vector", 
#which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  #Clear previous entries
  m <- NULL
  #set the value of the matrix
  set <- function(y) {
    #Assign value to x OUTSIDE current environment
    x <<- y
    m <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the value of the inverse
  setinverse <- function(inverse) m <<- inverse
  #get the value of the inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
####
#cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

#The following function calculates the inverse of the special "vector" that contains the matrix
#created with the above function. However, it first checks
#to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix 
#and sets the value of the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() #use precalculated data if available
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #Apply built-in matrix inversion function
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}


###################################################
#Sample calculations and associated results
#### First example
#This is an invertible matrix and is used as a sample input
mat <- matrix(c(3,1,-2,-2,0,3,0,1,0),3,3)
mat
#demo of built-in inverse function 
solve(mat)

b <- makeCacheMatrix(mat)
b$get()
#[,1] [,2] [,3]
#[1,]    3   -2    0
#[2,]    1    0    1
#[3,]   -2    3    0
b$getinverse()
#NULL
cacheSolve(b)
#[,1] [,2] [,3]
#[1,]  0.6    0  0.4
#[2,]  0.4    0  0.6
#[3,] -0.6    1 -0.4
b$getinverse()  # this is only to show you that the inverse has been stored and does not affect anything
#[,1] [,2] [,3]
#[1,]  0.6    0  0.4
#[2,]  0.4    0  0.6
#[3,] -0.6    1 -0.4
cacheSolve(b)
#getting cached data
#[,1] [,2] [,3]
#[1,]  0.6    0  0.4
#[2,]  0.4    0  0.6
#[3,] -0.6    1 -0.4

#### Second example
#This is an invertible matrix and is used as a sample input
mat <- matrix(c(4,1,-4,-2,0,5,0,6,0),3,3)
mat
#demo of built-in inverse function 
solve(mat)

b <- makeCacheMatrix(mat)
b$get()
#[,1] [,2] [,3]
#[1,]    4   -2    0
#[2,]    1    0    6
#[3,]   -4    5    0
b$getinverse()
#NULL
cacheSolve(b)
#[,1]      [,2]        [,3]
#[1,]  0.41666667 0.0000000  0.16666667
#[2,]  0.33333333 0.0000000  0.33333333
#[3,] -0.06944444 0.1666667 -0.02777778
b$getinverse()  # this is only to show you that the inverse has been stored and does not affect anything
#[,1]      [,2]        [,3]
#[1,]  0.41666667 0.0000000  0.16666667
#[2,]  0.33333333 0.0000000  0.33333333
#[3,] -0.06944444 0.1666667 -0.02777778
cacheSolve(b)
#getting cached data
#[,1]      [,2]        [,3]
#[1,]  0.41666667 0.0000000  0.16666667
#[2,]  0.33333333 0.0000000  0.33333333
#[3,] -0.06944444 0.1666667 -0.02777778
