## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

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


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

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
