## Put comments here that give an overall description of what your
## functions do

## Create a special matrix that can store its inverse as well
## to avoid possible repeats of long computations
makeCacheMatrix <- function(x = matrix()) {
	# Initialize the inverse to NULL
	invX <- NULL
	
	# Setter function for the matrix x
	set <- function(y) {
		# Set the matrix (in the parent i.e. makeCacheMatrix environment)
		x <<- y
		# Reset the inverse to NULL (out of sync from x)
		invX <<- NULL
	}
	
	# Getter for the matrix itself
	get <- function() x 
	
	# Setter for the inverse
	setinv <- function(inv) invX <<- inv  #in the parent i.e. makeCacheMatrix environment
	
	# Getter for the inverse
	getinv <- function() invX  
	
	# List the function withint this function
	list(set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)  
}


## Return the inverse of a special matrix if type makeCacheMatrix
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	
	
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	# Not chaced so calculate the inverse
	# Get the data
	data <- x$get()
	# Solve
	inv <- solve(data)
	# Set the inverse on the special matrix
	x$setinv(inv)
	
	# Return the inverse
	inv  
}
