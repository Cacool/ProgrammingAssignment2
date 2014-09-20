## creating a special invertible "matrix" object, then computes it's inverse 
## and return the result, if the inverse has already been calculated
## (and the matrix has not changed), then just return the cached result


## function makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	#set variable ivs (the inverse of a matrix in this case) to NULL
	ivs <- NULL

	#set function sets x to the argument y and set inv to null
	set <- function(y) {
		x <<- y
		ivs <<- NULL
	}

	#get function returns the value of x (argument of makeCacheMatrix)
	get <- function() x

	#sets ivs in makeCacheMatrix to inverse (the inverse of the matrix)	
	setinverse <- function(inverse) ivs <<- inverse

	# getinverse returns the value of ivs (from makeCacheMatrix)
	getinverse <- function() ivs

	#returns a list containing functions set, get, setinverse and  getinvserse
	list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse) 

} 


## function cacheSolve: This function computes the inverse of the 
## matrix created in makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the retrieve the cached inverse

cacheSolve <- function(x, ...) {
	#attempts to get the inverse of the matrix from x (if it was  calculated previously)
	ivs <- x$getinverse()

	 #if not null, a valued was cached, so return ivs
	if(!is.null(ivs)) {
		message("getting cached data")
		return(ivs)
	}

	#since its null, set data to x from makeCacheMatrix
	data <- x$get()

	#calculate the inverse of the matrix
	ivs <- solve(data, ...)

	#set ivs in x to calculated inverse
	x$setinverse(ivs)

	#return the inverse 
	ivs
}
