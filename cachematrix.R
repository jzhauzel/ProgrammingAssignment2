## My functions cache a matrix and it's inverse, returns the inverse,
## and let you know whether or not the matrix was
## already in the cache.

## This function takes a matrix and creates a global variable "mat",
## assigning it the value of the matrix and creates and assigns a global variable
## "inverse" to the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()){
	mat <<- x
	inverse <<- solve(mat)
}

## This function first determines if a matrix has already been cached
## to the global environment and if so, checks if its argument matches
## the cached matrix. Then, if the two match, produces the already computed
## inverse of the matrix. If the two matrices don't match or if a matrix has
## yet to be cached, then the function returns the inverse of the matrix.
## The function also tell you whether or not the matrix inverse was already cached.

cacheSolve <- function(x,...){
	if(exists("mat") == TRUE){
		compare <- identical(x,mat)
		if(compare == TRUE){
			print("getting cached data")
			return(inverse)
		}
		else{
			print("no cached data")
			return(solve(x))
		}
	}
	else{
		print("no cached data")
		return(solve(x))
	}
}
