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

##NOTE: See **below** some indications for testing this functions


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
## This function is directly connected with makeCacheMatrix
## It receives an object returned by makeCacheMatrix as a parameter
## This makes possible to use the Getters and setters as defined in the previous function
## when the value of "m" our global variable is not set("Is NULL"), the inverse of the Matrix
## is calculated, otherwise the value will be retrieved from cache and the "m" malue will be returned

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



# m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# m2 <- matrix(c(4, 2, 7, 6), nrow = 2, ncol = 2)
#
# Validate_m1_inv <- solve(m1)
# Validate_m2_inv <- solve(m2)
#
# aMatrix <- makeCacheMatrix(m1) ## Creates an Object type "MakeCacheMatrix" to be used as a paremeter when calling "cachesolve"
# aMatrix$get()               # retrieve the value of x. Since the makeCacheMatrix function was used, the return value will be x, that has the same Matrix given as parameter
# aMatrix$getinverse()           # retrieve the value of m, which is NULL because there is no (untill this point) inverse value stored in m.
# aMatrix$set(m1)          # set the matrix to the value of m1 to get its inverse (already confirmed it is a square invertible matrix)
# cacheSolve(aMatrix)          # Inverse is calculated from m1 for the first time. The answer (the inverse) is stored in "m". "m" is not NULL anymore
# aMatrix$getinverse()           # retrieve it directly, now that it has been cached
#
# ## Validate Function return match with the previously INV calculated (Only for validating purposes)
#
# identical(aMatrix$getinverse(), Validate_m1_inv)	## Validate if 2 matrix are identical. Answer is TRUE
#
# aMatrix$set(m2)         ## Set a new Matrix, so the inverse will ve calculated from this matrix
# aMatrix$getinverse()     ## Returns again NULL because the Matrix has changed. A new Inverse has to be calculated (for this Matrix) to be ables to get it ffrom Cache
# cacheSolve(aMatrix)	## Inverse of new Matrix (m2) is calculated
# aMatrix$getinverse()	## Get the inverse value from Cache. Since the value was already calculated, there is an answer
#
# ## Validate Function return match with the previously INV calculated (Only for validating purposes)
#
# identical(aMatrix$getinverse(), Validate_m2_inv)	## Validate if 2 matrix are identical. Answer is TRUE
