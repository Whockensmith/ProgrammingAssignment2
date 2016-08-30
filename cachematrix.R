## The functions below will calculate and cache the inverse of the matrix 
## Then retrieve the inverse matrix from cache

## The "get" function returns the vector x stored in the main function.
## The "set" function changes the vector stored in the main function.
## "get" and "set" do not calculate the mean, it only stores the value 
##      of the input as a variable "m".


makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
             x <<- y
             m <<- NULL
     }
       get <- function() x
       setinverse <- function(solve) m <<- solve
       getinverse <- function() m
       list(set = set, get = get,
                       setinverse = setinverse,
                       getinverse = getinverse)
}

## cacheSolve takes the cached inpute from makeCacheMatrix and Solves for the inverse

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)) {
             message("getting cached data")
             return(m)
      }
      data <- x$get()   ## Return a matrix that is the inverse of 'x'
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

## Type a<- diag(5,3) at the ">" propt
## This build the matrix
## Then type a at the ">"
## You will recieve the following matrix:

##      [,1] [,2] [,3]
## [1,]    5    0    0
## [2,]    0    5    0
## [3,]    0    0    5

## To cache the matrix "a" type CachedMarix <- makeCacheMatrix(a) at the propt
## To get the inverse of "a" and display it type cacheSolve(CachedMarix) at promt

## You will get

## getting cached data
##      [,1] [,2] [,3]
## [1,]  0.2  0.0  0.0
## [2,]  0.0  0.2  0.0
## [3,]  0.0  0.0  0.2

## You can validate this using
## solve(diag(5,3))
