## Matrix inversion is a complex computation of high cost. Therefore
## with the below functions one can create a cache object and 
## then calculate its inverse

## This function is used to get, set a matrix object or its inverse 
## in cache

makeCacheMatrix <- function(x = matrix()) {
        # Initialize inverse variable
        i <- NULL
        # Set the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL # initialize inverse of new matrix
        }
        # Get the matrix
        get <- function() x
        # Set the inverse of a matrix
        setinverse <- function(inverse) i <<- inverse
        # Get the inverse of a matrix
        getinverse <- function() i
        # List all possible operations
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function inverts a matrix object. If the inverse is already
## in cache, then it returns the cache value.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

        ## Get the matrix inverse from cache and store it into variable i
        i <- x$getinverse()
        
        ## if already in cache, then return the cached value
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        # Otherwise, get the matrix object
        data <- x$get()
        # Invert the matrix object
        i <- solve(data, ...)
        # Save the inverse of the matrix object in cache
        x$setinverse(i)
        # Return the inverse
        i
}
