# Function to create a special "matrix" object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Initialize an empty cache to store the inverse matrix
    inverse <- NULL
    
    # Function to set the matrix
    set <- function(matrix) {
        x <<- matrix  # Using <<- to assign to the parent environment
        inverse <<- NULL  # Reset the cache when the matrix is set
    }
    
    # Function to get the matrix
    get <- function() {
        x
    }
    
    # Function to get the cached inverse if available; otherwise, compute and cache the inverse
    cacheSolve <- function(...) {
        if (!is.null(inverse)) {
            message("Getting cached inverse")
            return(inverse)
        }
        
        # Compute the inverse using solve() and cache it
        inverse <<- solve(x, ...)
        inverse
    }
    
    # Return a list containing the functions
    list(set = set, get = get, cacheSolve = cacheSolve)
}

# Function to compute the inverse of the special "matrix" object
cacheSolve <- function(cacheMatrix, ...) {
    cacheMatrix$cacheSolve(...)
}
# Create a special "matrix" object that caches its inverse
m <- makeCacheMatrix(matrix(1:4, 2, 2))

# Get the matrix
m$get()
# Output:
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# Compute and cache the inverse
m$cacheSolve()
# Output:
#            [,1] [,2]
# [1,] -2.0000000  1.5
# [2,]  1.0000000 -0.5

# Retrieve the cached inverse
m$cacheSolve()
# Output:
# Getting cached inverse
#            [,1] [,2]
# [1,] -2.0000000  1.5
# [2,]  1.0000000 -0.5
