## To avoid costly re-computation of large inverse matrices, it is possible to cache the inverse matrix. 
## This is done by creating an object like environment (makeCacheMatrix) where the inverted matrix can be stored,
## together with functions to access (read, write) the original matrix as well as the inverse matrix.

## -----------------------------------------------------------------------------------------------------------------
## makeCacheMatrix implements the access functions for the matrices (mat,invertedMatrix) as well as the environment
## to store both matrices. 
## makeCacheMatrix returns a list of access functions.

## setMatrix: stores the matrix and initializes invertedMatrix.
## setInvertedMatrix:  stores invertedMatrix.
## getMatrix: retrieves the matrix.
## getInvertedMatrix: retrieves the invertedMatrix.
## -----------------------------------------------------------------------------------------------------------------
makeCacheMatrix <- function(mat = numeric()) {
    invertedMatrix <- NULL
    ## set and get matrix mat      
    setMatrix <- function(m) {
        mat <<- m
        invertedMatrix <<- NULL
    }
    
    getMatrix <- function() mat
    
    ## set and get inverted Matrix
    setInvertedMatrix <- function(inverted) invertedMatrix <<- inverted
    
    getInvertedMatrix <- function() invertedMatrix
    
    ## return set and get functions for matrix mat and invertedMatrix
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInvertedMatrix = setInvertedMatrix,
         getInvertedMatrix = getInvertedMatrix)
}

## -----------------------------------------------------------------------------------------------------------------
## cacheSolve checks whether the inverted matrix is already calculated, if so, invertedMatrix is returned
## if not, invertedMatrix is calculated, cached and returned
## It is assumed that the original matrix can be inverted. 
## -----------------------------------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {

    invertedMatrix <- x$getInvertedMatrix()
    
    if(!is.null(invertedMatrix)) {
        message("getting cached data")
        return(invertedMatrix)
    }
    data <- x$getMatrix()
    invertedMatrix <- solve(data)
    x$setInvertedMatrix(invertedMatrix)
    invertedMatrix
}
