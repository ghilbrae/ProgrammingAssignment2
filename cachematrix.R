## The following functions are intended for calculating and storing the inverse
## of a matrix. Their main goal is to cache this data in a way that can be 
## easily accessible after it has been computed once, and in doing so saving in 
## computing time and resources.

## This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # This function creates a special "matrix" object that can cache its inverse.
    #
    # Args:
    #   x: A matrix object whose inverse is going to be calculated and cached
    #   
    # Returns:
    #   A list of functions to access and store the inverse of the matrix
    #   
    
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    #get <- function() x
    get <- function(){            #Alternative writing for easier reading
        x
    }
    #setsolve <- function(solve) s <<- solve
    setsolve <- function(solve){  #Alternative writing for easier reading
        s <<- solve
    }
    #getsolve <- function() s
    getsolve <- function(){       #Alternative writing for easier reading
        s
    }
    # Return a list of the functions available
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function computes the inverse of a matrix returned by `makeCacheMatrix`.
## If the inverse has already been calculated (and the matrix has not changed), 
## then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # This function computes the inverse of a matrix returned by `makeCacheMatrix`.
    # If the inverse has already been calculated (and the matrix has not changed), 
    # then `cacheSolve` should retrieve the inverse from the cache.
    #
    # Args:
    #   x: A list of functions that enable access to the inverse of a matrix
    #
    # Returns:
    #   Return a matrix that is the inverse of a given matrix stored by 
    #   makeCacheMatrix

    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
