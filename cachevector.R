# Returns an object which has to be stored somewhere to be accessible later.
# For testing try:
# vector <- makeVector(c(1, 2, 3))
# cachemean(vector)
# cachemean(vector)
# vector$set(c(4, 5, 6))
# cachemean(vector)
# cachemean(vector)
# cachemean(vector)

makeVector <- function(x = numeric()) {
    # In this function, four functions are defined:
    # get, set, getmean and setmean
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #get <- function() x
    get <- function(){
        x
    }
    #setmean <- function(mean) m <<- mean
    setmean <- function(mean){
        m <<- mean
    }
    #getmean <- function() m
    getmean <- function(){
        m
    }
    # Return a list of functions available
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
