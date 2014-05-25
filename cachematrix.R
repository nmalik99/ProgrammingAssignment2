## Create a caching envirnment for a matrix and its inverse
## store matrix alongwith its inverse in a single object 
## which is a list of 4 functions. 

## Take a matrix and return a list containing 4 functions to set and get source matrix 
## and set and get corresponding inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## Expects cache matrix object created by makeCacheMatrix function as input
## and return inverse. If the inverse was done previously it will use cached inverse matrix
## it will calc inverse, save it in the cache matrix object and return inverse matrix

cacheSolve <- function(x, ...) {
		m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m        
}
