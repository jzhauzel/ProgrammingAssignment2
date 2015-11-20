## My functions cache a matrix and it's inverse, returns the inverse,
## and let you know whether or not the matrix was
## already in the cache.

## This function takes a matrix and creates a global variable "mat",
## assigning it the value of the matrix and creates and assigns a global variable
## "inverse" to the inverse of the matrix.

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

## This function first determines if a matrix has already been cached
## to the global environment and if so, checks if its argument matches
## the cached matrix. Then, if the two match, produces the already computed
## inverse of the matrix. If the two matrices don't match or if a matrix has
## yet to be cached, then the function returns the inverse of the matrix.
## The function also tell you whether or not the matrix inverse was already cached.

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
