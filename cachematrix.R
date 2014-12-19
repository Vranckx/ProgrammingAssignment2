## Set of two functions that respectively:
## 1) Create a list of functions that can define/compute and return a matrix and its inverse, and cache them ("makeCacheMatrix")
## 2) Return the inverse of a matrix, by either getting it from the cache (if available) or computing it

## Define a list of functions that respectively set and get the value of a matrix "initial_matrix" and of its inverse "inverse_matrix")
makeCacheMatrix <- function(initial_matrix = matrix()) {	
	
	## First, explicitely define the initial value of the local variable "inverse_matrix" as NULL
	inverse_matrix <- NULL

	## Define function "set", which takes a matrix as argument and stores it in the global "initial_matrix" variable
	## It also erases any cached "inverse_matrix" value since the initial matrix has now changed
	set <- function(y) {
		initial_matrix <<- y
		inverse_matrix <<- NULL
	}

	## Define function "get", which simply gives "initial_matrix"
	get <- function() initial_matrix

	## Define function "setinverse", which computes the inverse of a matrix and stores it globally in "inverse_matrix" 
	setinverse <- function(solve) inverse_matrix <<- solve 

	## Define function "getinverse", which simply returns the "inverse_matrix"
	getinverse <- function() inverse_matrix

	## Store these four functions in a list
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function that returns a matrix that is the inverse of input "initial_matrix", but only computes it if it has not already been cached
cacheSolve <- function(initial_matrix, ...) {	
	
	## Get the inverse of "initial_matrix" using function "getinverse"
	inverse_matrix <- initial_matrix$getinverse()

	## First, check if the inverse "inverse_m" is cached and, if so, return it and do not go any further
	if(!is.null(inverse_matrix)){
		message("Found cached inverse matrix:")
		return(inverse_matrix)
	}

	## Otherwise, get the matrix and store it in local variable "data", compute its inverse, store it and return it
	data <- initial_matrix$get()
	inverse_matrix <- solve(data, ...)
	x$setinverse(inverse_matrix)
	inverse_matrix
}
