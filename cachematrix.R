## Function to create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	my_matrix <- NULL
	set <- function(y){
		x <<- y
		my_matrix <<- NULL
	}
	get <- function() x
	set_inverse <- function(solve) my_matrix <<-solve
	get_inverse <- function () my_matrix
	list (set = set, get = get,
			set_inverse = set_inverse,
			get_inverse = get_inverse)
}

## Function to compute the inverse of matrix returned by makeCacheMatrix. 
## If inverse already calculated (and matrix remains unchanged), then 
## cacheSolve should get the inverse from cache.

cacheSolve <- function(x, ...) {
        my_matrix <- x$get_inverse()
        if(!is.null(my_matrix)){
        	message("getting cached data")
        	return(my_matrix)
        }
        data <- x$get()
        my_matrix <- solve(data, ...)
        x$set_inverse(my_matrix)
        my_matrix
}

