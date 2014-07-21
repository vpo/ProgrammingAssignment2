## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this cache matrix object provides 
## - a setter method to set the matrix which should be inversed
## - getter method get the cached inverse matrix
## - setter method to set the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix
    getInverseMatrix <- function() m
    list(set=set, get=get,
         setInverseMatrix=setInverseMatrix,
         getInverseMatrix=getInverseMatrix)
}


## Write a short comment describing this function
## this function needs a matrix object as first parameter and 
## computes the inverse matrix from the matrix contained by matrix object
## if the inverse matrix is already computed and cached, the cached one 
## will be returned
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverseMatrix()
    if (!is.null(m)) {
        message("getting cached data")
        return (m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setInverseMatrix(m)
    m
}
