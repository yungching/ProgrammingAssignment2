## The makeCacheMatrix function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function (x = matrix()) {
		m <- NULL 
		set <- function(y) {
	  			x <<- y
				m <<- NULL 
      	}                                   # set the value of the matrix
      	get <- function() x                 # get the value of the matrix
      	setinv <- function(inv) m <<- inv   # set the value of the inverse of the matrix
      	getinv <- function() m              # get the value of the inverse of the matrix
      	list(set = set, get = get,
			setinv = setinv,
	       	getinv = getinv)                # return a list containing the function to cache the inverse of the matrix
}

## The cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix function 

cacheSolve <- function(x, ...) {
		m <- x$getinv()                         # get the value of the inverse of the matrix from x
		if(!is.null(m)) {                       # if the inverse has already been calculated
				message("getting cached data")
				return(m)                       # retrieve the inverse from the cache
		}
		data <- x$get()                         # if the inverse has not been calculated
		m <- solve(data)                        # then we compute the inverse of the matrix
		x$setinv(m)                             
		m                                       # retrieve the computed result
}