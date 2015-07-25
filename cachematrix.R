## Holds matrix input and output
makeCacheMatrix <- function(input = matrix()) {
	## Define output variable
	cached <- NULL
	
	## Getter
	get <- function() input
	
	## Setter
	set <- function(newInput) {
		input <<- newInput
		cached <<- NULL
	}
	
	## Inversed getter
	getInversed <- function() cached
	
	## Inversed setter
	setInversed <- function(inversed) cached <<- inversed
	
	## Expose methods
	list(
		set = set,
		get = get,
		getInversed = getInversed,
		setInversed = setInversed
	)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	cached <- x$getInversed()
	
	if (!is.null(cached)) {
		message('Cached data found!')
		return(cached)
	}
	
	input <- x$get()
	
	## Call the target function
	inversed <- solve(input, ...)
	x$setInversed(inversed)
	
	inversed
}
