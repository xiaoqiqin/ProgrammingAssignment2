## The following two functions create a special object that stores a matrix
## and cache its inverse.
## Note that we assume the input matrix is always invertable.
#-------------------------------------------------------------------------------------#
# makeCacheMatrix() creates a special "matrix" object,
# which is a list that contains the following functions:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix
#-------------------------------------------------------------------------------------#
## input for this function:  a matrix
## output from this function: a list
#-------------------------------------------------------------------------------------#
makeCacheMatrix <- function(x = matrix()) {
    ##initialize the inverseMatrix object
    inverseMatrix <- NULL
    ## function that set the input matrix
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    ## functiont that get the input matrix
    get <- function() x
    ## function that set the inverse matrix
    setinverse <- function(inverse) inverseMatrix <<- inverse
    ## functiont that get the inverse matrix
    getinverse <- function() inverseMatrix
    ## returns a list that contains the above functions
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

#-------------------------------------------------------------------------------------#
# cacheSolve() computes the inverse of the special "matrix" object
# which is created by the above makeCacheMatrix() function.
# However, if the inverse has already been calculated
# and the matrix has not changed, then in stead of calculate the inverse again,
# cacheSolve() just retrieve the inverse value from the cache.
#-------------------------------------------------------------------------------------#
## input for this function: makeCacheMatrix (a list containing a matrix and functions)
## output for this function: a matrix (inverse of the input matrix)
#-------------------------------------------------------------------------------------#
cacheSolve <- function(x) {
    ## get the inverseMatrix value in list x
    inverseMatrix <- x$getinverse()
    ## if the inverseMatrix in not NULL, which means it has been calculated
    if(!is.null(inverseMatrix)) {
        message("calculated earlier, getting cached data")
        return(inverseMatrix)
    }
    ## if the inverse matrix is not available, get the original matrix
    matrix <- x$get()
    ## caculate the inverse matrix
    inverseMatrix<- solve(matrix)
    ## store the inverse matrix in the list
    x$setinverse(inverseMatrix)
    ## return the calculated inverse matrix
    inverseMatrix
}
