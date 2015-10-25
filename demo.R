#In this example we introduce the <<- operator which can be used to assign a value
#to an object in an environment that is different from the current environment. 
#Below are two functions that are used to create a special object
#that stores a numeric vector and cache's its mean.

#The first function, makeVector creates a special "vector", 
#which is really a list containing a function to
    #set the value of the vector
    #get the value of the vector
    #set the value of the mean
    #get the value of the mean

makeVector <- function(x = numeric()) {
  m <- NULL
  #set the value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get the value of the vector
  get <- function() x
  #set the value of the mean
  setmean <- function(mean) m <<- mean
  #get the value of the mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #set the value of the matrix
  set <- function(y) {
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

#The following function calculates the mean of the special "vector"
#created with the above function. However, it first checks
#to see if the mean has already been calculated. 
#If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the mean of the data 
#and sets the value of the mean in the cache via the setmean function.
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}

#Example from https://asitarrives.wordpress.com/2014/10/18/understanding-lexical-scoping-in-r-great-guidance-for-community-ta-in-coursera/comment-page-1/#comment-12
a <- makeVector(c(1,2,3,4))
a$get()
#[1] 1 2 3 4
a$getmean()
#NULL
cachemean(a)
#[1] 2.5
a$getmean()  # this is only to show you that the mean has been stored and does not affect anything
#[1] 2.5
cachemean(a)
#getting cached data
#[1] 2.5

a$set(c(10,20,30,40))
a$getmean()
#NULL
cachemean(a)
#[1] 25
cachemean(a)
#getting cached data
#[1] 25
a$get()
#[1] 10 20 30 40
a$setmean(0)  # do NOT call setmean() directly despite it being accessible for the reason you will see next
a$getmean()
#[1] 0      # obviously non-sense since.
a$get()
#[1] 10 20 30 40
cachemean(a)
#[1] 0    # as you can see the call to setmean() effectively corrupted the functioning of the code
a <- makeVector(c(5, 25, 125, 625))
a$get()
#[1] 5 25 125 625
cachemean(a)
#[1] 195
cachemean(a)
#getting cached data
#[1] 195


a <- makeVector(rnorm(2, -10, 20))
a
a$get()
#[1] -23.93434  29.19192
a$getmean()
#NULL
cachemean(a)
#[1] 2.628789
cachemean(a)
#getting cached data
#[1] 2.628789
a$setmean(3)
cachemean(a)
#getting cached data
#[1] 3

#Sample calculations
#This is an invertible matrix and is used as a sample input
mat <- matrix(c(3,1,-2,-2,0,3,0,1,0),3,3)
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
