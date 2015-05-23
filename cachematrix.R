## The functions below are used to create a special object
## that stores a numeric matrix and caches its inverse 

## The function makeCacheMatrix creates a special "matrix",
## which is really a list list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	I <- NULL
	set <- function(y){
		x <<- y
		I <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) I <<- inverse
	getinverse <- function() I
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of the
## special "matrix" created with the above function. It first
## checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse og the
## data and sets the value of the inverse in the cache via the
## setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinverse()
        if(!is.null(I)){
        	message("getting cached data")
        	return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
        I
}