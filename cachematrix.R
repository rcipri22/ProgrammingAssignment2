## makeCacheMatrix - calculates and caches the inverse of a matrix
## cacheSolve

## makeCacheMatrix - calculates the inverse of a matrix and 
## caches the inverse of a matrix -- using '<<-' in a special location

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL # The cache location for matrix is set to NULL
        set <- function(y) { # a new function is defined that caches x<<-x
                x <<- y      
                m_inv <<- NULL
        }
        get <- function() x # get the value of the matrix
        setinverse <- function(solve) m_inv <<- solve #set value inverse
        getinverse <- function() m_inv # get value of inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the mean of a function or gets the value from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setinverse(m)
        
        m
        
}
