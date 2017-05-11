## These functions provide a list which can store the inverse of a matrix

## makeCacheMatrix creates an R object that stores a matrix and its inverse.
## It requires a matrix as an input.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve requires the object created by makeCacheMatrix, will check that
## object to see if the inverse is stored in the makeCacheMatrix environment.
## If the inverse has not been calculated yet, calculates the inverse and sets
## it in the makeCacheMatrix environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse() ## gets the item titled getinverse
        if(!is.null(i)) { ## checks to see if the item is not null
                message("getting chached data") ## states it is getting the cached item
                return(i) ## returns the item
        }
        data <- x$get() ## sets data to the item titled get
        i <- solve(data, ...) ## solves for the inverse of the matrix
        x$setinverse(i) ## sets the inverse in cache environment
        i ## returns the inverse
}
