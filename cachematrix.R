## Functions to manage a cache of inverted matrices.   
## These can be be used to eliminated duplicate computation and
## hide the logic for first time calculation and cache access
## from the calling program

## Create a list of methods to cache a matrix and inverted solution

makeCacheMatrix <- function(x = matrix()) {
        inverted_x  <- NULL
        
        set <- function(y) {
                x <<- y
                inverted_x <<- NULL
        }
        
        get <- function() x
        
        setim <- function(im = matrix) inverted_x <<- im
        
        getim <- function() inverted_x
        
        
        list(set = set, 
             get = get,
             setim = setim,
             getim = getim     )
        }



## Function used to access cache.
## On first access solve and cache the inverted matrix

cacheSolve <- function(x, ...) {
        im <- x$getim()
        if(is.matrix(im)) {
                message("matrix cache hit")
                return(im)
        }
        message("first time matrix inversion")
        mx <- x$get()
        im <- solve(mx, ...)
        x$setim(im)
        im
        }
