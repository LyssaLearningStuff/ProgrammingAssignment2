##The functions below store the inverse of a specially created matrix 
##so that the specific value can be used again without having to be 
##recalculated every time. If the values within the matrix are changed,
##the functions account for that change by recomputing the inverse.  


##This function sets the foundation by creating the special matrix.
##After setting and getting the value of the matrix, it then gets and 
##sets the value of the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<-NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function is used to return the inverse of the matrix created in the 
##above function. It determines whether or not the inverse has already been 
##computed and stored. If so, it does not recompute the inverse, but provides
## the stored value and informs the user that the value is cached data.  


cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
