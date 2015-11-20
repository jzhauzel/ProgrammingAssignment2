## My functions cache a matrix and it's inverse, returns the inverse,
## and let you know whether or not the matrix was
## already in the cache.

## The first function creates a special matrix which is a list that contains functions to 
## set/get/set inverse/get inverse values of a mtrix.

makeCacheMatrix <- function(x = matrix()){
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# This second function calculates the value of the inverse of the matrix, but first
# checks to see if it has already been done. If so it returns the value and if not 
# calculates the value of the inverse and then returns it.

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
