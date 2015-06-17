## This assignment explores matrix inversion. Matrix inversion takes a lot of computing power. So, it is easier to cache the 
## value of the inverse matrix instead of having it computed every time the value is necessary. These two functions are used to
## create a matrix, get the inverse, and cache the matrix. The end goal of these functions is to save computing power.

## The first function is "makeCacheMatrix." This function creates a list that does the following:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the inverse of the matrix
## 4. gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        }
 
        

## The next function is "cacheSolve". This function returns the inverse of the matrix. The first thing cacheSolve does is 
## check the inverse that was created in the function "makeCacheMatrix." Since the "makeCacheMatrix already computed the
## inverse, the function "cacheSolve" doesn't need to do the computation. In the case that the inverse has not been computed,
## the "cacheSolve" function will compute the inverse and set the value in the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data.")
                return(inverse)
                }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
        }
