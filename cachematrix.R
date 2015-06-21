#makeCachMatrix creates the matrix and lists the set of functions
makeCacheMatrix <- function(x = matrix()) {
#inv sets the matric to null
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        #returns the inverse of the matrix
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        #lists the functions 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## cacheSolve solves the inverse of the matrix using solve() function
cacheSolve <- function(x, ...) {
##checks if the inverse of matrix is available
        inv <- x$getinv()
        ##checks if the matrix is null matrix, if the matrix is not a null matrix then it retrieves the inverse from cache
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        ## if the inverse matrix doesnot exists, creates the inverse matrix
        data <- x$getinv()
        ## computes the inverse of the matrix using sovle function
        inv <- solve(data)
        ## sets the inverse matrix in cache
        x$setinv(inv)
        ## returns the inverse matrix
        return(inv)
}
