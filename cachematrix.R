## makeCacheMatrix take a matrix as input, and return a list of functions the will be used in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inverse_matrix <<- inverse
        get_inverse <- function() inverse_matrix
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## cacheSolve calculates the inverse of a matrix: before calculating, the function checks if the 
## calculation have been completed before: if yes, the function return the value previously stored
## (cached); if not, the function calculates and returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$get_inverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$set_inverse(inverse_matrix)
        inverse_matrix
}
