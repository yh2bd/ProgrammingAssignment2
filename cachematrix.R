## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    
    setMatrix <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    
    getMatrix <- function() x # get Matrix
    setInverse <- function(inverse) invMatrix <<- inverse # set invertible matrix
    getInverse <- function() invMatrix # get the invertible matrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,
    setInverse = setInverse, getInverse = getInverse)
    
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) { # if the inverse matrix not null
            message("Getting Cached Invertible Matrix")
            return(invMatrix) #return invertible matrix
        }
        
        
        MatrixData <- x$getMatrix() # get originial Matrix
        invMatrix <- solve(MatrixData, ...) # inverse the Matrix
        x$setInverse(invMatrix) # set the invertible Matrix
        return(invMatrix) # return
}

