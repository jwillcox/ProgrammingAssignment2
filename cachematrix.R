## The following functions are used to cache the computation of the inverse
## of a square matrix

## The makeCacheMatrix function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## sets variable m to NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        ## assigned 'y' to 'x' and 'm' to 'NULL' in the parent environment
        get <- function() x
        ## assigns 'x' to get
        setsolve <- function(solve) m <<- solve
        ## When the cacheSolve function is run, will assign the inverse
        ## of the matrix to 'setsolve' as well as to 'm'
        getsolve <- function() m
        ## assigns 'm' to getsolve
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The cacheSolve function computs the inverse of the matrix returned by the
## makeCacheMatrix function. If the inverse has already been calculated,
## the cacheSolve function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve() 
        ## assigns getsolve from makeCacheMatrix to variable m. The first time
        ## this function is run, the result will be NULL
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ## if variable m is not null, it returns the message "getting cached
        ## data" as well as the inverse of the matrix. If variable m is null,
        ## it will not perform these steps and instead will continue in the code
        data <- x$get()
        ## assigns get from makecacheMatrix to variable data
        m <- solve(data, ...)
        ## assigns the inverse of the matrix to variable m
        x$setsolve(m)
        ## assigns the inverse of the matrix to the makeCacheMatrix$setsolve
        m
        ## returns the inverse of the matrix
}
