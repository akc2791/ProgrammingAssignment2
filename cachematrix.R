## The two functions here peform 2 jobs - 1st function caches the inverse of a matrix 
## and the second one checks the cache and outputs the result. Detailed explanations follow

## This function takes the inverse of a square matrix and stores the result in a cache variable.
## This cache variable are stored in a environment which is diff from that of the function.

makeCacheMatrix <- function(x=matrix()) {    #making the function
    m <- NULL                       
    set <- function(y) {            #making the set function
        x <<- y
        m <<- NULL
    }
    get <- function() x             #
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function checks the cached variables and if the answer is available in the variable in it then it just takes it from there.
## else it calculates the inverse of the variable

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}