## Put comments here that give an overall description of what your
## functions do
#Lexical Scoping
## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inversed <- NULL
	
    set <- function(y) {
        x <<- y
        inversed <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inversed <<- inverse
    retrieveinverse <- function() inversed
    list(set=set, get=get, setinverse=setinverse, retrieveinverse=retrieveinverse)
}

## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inversed <- x$retrieveinverse() 
    if(is.null(inversed)) {
        #continue to retrieve the cache data.
    }
	else {
		#do nothing if inversed is already exist, just return the cached data
        return(inversed)
	}
    data <- x$get()
    inversed <- solve(data)
    x$setinverse(inversed)
    inversed #return the inversed of the data
}
