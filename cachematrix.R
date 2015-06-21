##ProgrammingAssignment2

## Below are two functions that are used to create an
## object that stores a matrix and cache's its inverse


## 	In this function, makeCacheMatrix creates a matrix 
##  that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y) {
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinverse<-function(inverse) m<<-inverse
	getinverse<-function() m
	list(set=set, get=get, 
	setinverse=setinverse, 
	getinverse=getinverse)
}


## In this function, cacheSolve first check whether
## the inverse has been stored in the cache, if yes,
## then gets the inverse without calculation, if no,
## computes the inverse and store it in the cache.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m<-x$getinverse()
    ## Check if the inversed matrix has already been calculated, and if so, get the inverse from cache
	if(!is.null(m) {
		message('getting cached data')
		return (m)
	}
    ##calculates the inversed matrix using solve function
	data<-x$get()
	m<-solve(data,...)
}

