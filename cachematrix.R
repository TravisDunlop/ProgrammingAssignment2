## This series of functions implements R's solve to invert matrix
## with the added capability of caching previously solved answers.

## This function creates an object analagous to a matrix that can
## store cached values.
## set and get deal with the matrix itself while
## getInverse and setInverse get and set the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(i) inverse <<- i
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function is analagous to R's solve function
## but first it checks to see if the makeCacheMatrix
## object already has a value of the inverse cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
                message("Getting Cached Data...")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
