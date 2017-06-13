## Put comments here that give an overall description of what your
## functions do
## This functions are used to calculate the Inverse of a Matrix a square invertible matrix
## Since doing this calculation verry often might required a lot of processing time,
## We can store the Inverse in cache, so the calculation is not done everytime, but only
## when the Matrix is different. This reduces the processing time and facilitates the work
## 2 Functions are used for this: "makeCacheMatrix" and "cacheSolve". The first created an
## object type "makeCacheMatrix"and builds the functions normally called "getters and settters"
## Which will be used to set and get the information
## The function "cacheSolve", as its name implies, solves the Matrix and uses the getters and setters
## to write in an object savd in the Parent environment "m" (Which will cache (store) the Inverse every time)
## a new matrix is set


## Write a short comment describing this function
## Creates an Object Type "MakecacheMatrix" that is required to call the second function "cacheSolve"
## In this function a list is created to facilitate the access to the objects (for set and get)
## The value of "m" where the inverse is stored, is initialized to NULL but later on it will contain
## the inverse value, avoiding the continuos inverse calculation
## inverse should be calculated again when the "x"value is reset, threfore "m" is again initialized with NULL untill
## A new inverse calculation is made

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        ## here the Inverse is calculated
        m <- solve(data)

        x$setinverse(m)
        m
}
