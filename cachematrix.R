## Create a list of functions that provide framework for storing the inverse of a matrix,
## then create a function that uses that list to retrieve/calculate the inverse and store it

## Define a list of functions that can be used to retrieve or assign matrix/inverse values

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Use functions from 'makeCacheMatrix' to retrieve the stored inverse of the matrix, 'x',
## or to calculate the inverse and then store it

cacheSolve <- function(x, ...) {
       i <- x$getinverse()
       if(!is.null(i)) {
             message ("getting cached data")
             return(i)
       }
       data <- x$get()
       i <- solve(data, ...)
       x$setinverse(i)
       i
        ## Return a matrix that is the inverse of 'x'
}
