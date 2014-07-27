
## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## The input for makeCacheMatrix is to define a matrix, for example: makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes, caches, and returns the inverse of the matrix made by makeCachematrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m   
}
