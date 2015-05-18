## The two functions below are used to create an object that stores a matrix
## and caches the inverse of the matrix. Used to prevent computing the inverse repeatedly
## when large matrices are used.
## Note: these functions assume the matrix supplied is always invertible

## Creates a list of functions to create a special 'matrix' object that can cache its inverse
## This function will: 
##  - Set the matrix
##  - Get the matrix
##  - Set the inverse of the matrix
##  - Get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  ## Set the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x   ## Get the matrix
        setinverse <- function(solve) m <<- solve ## Set the inverse matrix
        getinverse <- function() m   ## Get the inverse matrix
        list(set = set, get = get, ## List all functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of a matrix created with makeCacheMatrix either through computation
## using solve() or by retrieving it from the cache if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {              ##if matrix is defined return from cache
                message("getting cached data") 
                return(m)
        }
        data <- x$get()               ##if matrix is not defined, solve and store in cache
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

##Test case:
##> mat <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
##> mat
##[,1] [,2]
##[1,]    4    7
##[2,]    2    6
##> mat2 <- makeCacheMatrix(mat)
##> cacheSolve(mat2)
##[,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4
##> cacheSolve(mat2)
##getting cached data
##[,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4
