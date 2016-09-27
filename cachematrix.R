## R Programming - Assignment 2 : Lexical Scoping

## The 'n' x 'n' matrix is required to ensure the
## correct output is produce.
## 
## example
##
## |4 3| ==> |-2  3|  
## |3 2|     | 3 -4|
##
## $> m <- makeCacheMatrix(matrix(c(4,3,3,2),nrow=2,ncol=2))
## $> cacheSolve(m)
## $     [,1] [,2]
## $[1,]   -2    3
## $[2,]    3   -4

## Function which accept a matrix as input
makeCacheMatrix <- function(x = matrix()) {
    ## initialize the inverse matrix variable 'm' with NULL.
    m <- NULL
    
    ## set the value of the matrix and initialize inverse matrix
    ## variable 'm' to NULL.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get the value of the matrix
    get <- function() x
    
    # set the value of the inverse matrix
    setInverse <- function(solve) m <<- solve
    
    ## get the value of the inverse matrix
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function that inverse the given matrix from makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## get the inverse matrix from makeCacheMatrix.
    m <- x$getInverse()
    
    ## verify the inverse matrix is already in cache and return
    ## the value.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## get the matrix and use inverse it using solve() function
    data <- x$get()
    m <- solve(data, ...)
    
    ## assign the inverse matrix to cache
    x$setInverse(m)
    
    ## return inverse matrix
    m
}
