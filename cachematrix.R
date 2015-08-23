## Some functions that cache the inverse of a given matrix
##########################################################

## makeCacheMatrix makes a matrix object that caches its inverse
makeCacheMatrix <- function(MTRX = matrix()) {
## Initialize i
i <- NULL
## Sets the matrix
setMatrix <- function( matrix )
{
MTRX <<- matrix
i <<- NULL
}
## Gets the matrix
getMatrix <- function()
{
MTRX
}
## Sets inverse of matrix
setInverse <- function(inverse)
{
i <<- inverse
}
## Gets inverse of the matrix
getInverse <- function()
{
i
}

## Return list of the methods
list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


#########################
## Solves and returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
MTRX <- x$getInverse()
## If matrix hasn't changed, just return it.
if( !is.null(MTRX) )
{
message("getting cached data")
return(MTRX)
}

data <- x$get()

## Solve for inverse
MTRX <- solve(data) %*% data
x$setInverse(x)

## Return inverse matrix.
MTRX
}

## The End
