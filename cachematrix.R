## Functions used for matrix caching (makeCacheMatrix) and matrix inversion (cacheSolve).

## makeCacheMatrix:	Closure function encapsulating matrix object along
##			with generic cached result and operations to allow:
##			* get/set of input matrix
##			  (with appropriate changes to cached matrix)
##			* getCache/setCache of cached matrix result
##
## Example usage:
##	a <- makeCacheMatrix( matrix(c(1,3,4,5,2,1,6,5,2),3,3) )
##
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL

	## Input matrix set / get functions:
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x

	## Cached matrix set / get functions:
	setCache <- function(toSet) m <<- toSet
	getCache <- function() m

	# Result as a list encapsulating required operations and environments:
	list( get = get, set = set,
		setCache = setCache,
		getCache = getCache)
}


## cacheSolve:	Function to calculate the inverse of a matrix 'x'. Caches result
##		after first computation. Will only ever execute the solve
##		function once for a specific matrix 'x'.
##
## Example usage (list "a" set using "makeCacheMatrix" function, as above):
##	cacheSolve( a )
##
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	## Input object needs to be created using 'makeCacheMatrix' function
	## (must include get / getCache / setCache functions)

	## Check if result is in cache, if so return cached result
	m <- x$getCache()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	## If result not available from cache:
	data <- x$get()			# Read input matrix from environment
	m <- solve(data, ...)		# Calculate inverse
	x$setCache(m)			# Set cache based on result (for future)
	m				# Return calculated result
}
