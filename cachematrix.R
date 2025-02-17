## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special "matrix" object that can cache 
## its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        
        ## cache_matrix stores the cached inverse matrix (initialized as NULL)
        cache_matrix <- NULL
        
        ## updates the matrix and clears the cached one
        set <- function(y) {
                x <<- y
                ## modify the variable in the parent environment
                cache_matrix <<- NULL 
        }
        
        ## returns the current matrix in the cache
        get <- function() x
        
        ## Stores a calculated inverse matrix in the cache
        setmatrix <- function(inverse_matrix) cache_matrix <<- inverse_matrix
        
        ## Retrieves the cached inverse matrix
        getmatrix <- function() cache_matrix
        
        list(set = set, get = get, 
               setmatrix = setmatrix,
               getmatrix = getmatrix)
}


## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache_matrix <- x$getmatrix()
        if(!is.null(cache_matrix)) {
                message("getting cached data")
                return (cache_matrix)
        }
        data <- x$get()
        cache_matrix <- solve(data, ...)
        x$setmatrix(cache_matrix)
        cache_matrix
}
