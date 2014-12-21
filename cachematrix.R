## Put comments here that give an overall description of what your
## functions do

## Return a matrix object with it's inverse cached as "inverse".
makeCacheMatrix <- function(x = matrix()) {
            i <- NULL
            set <- function(y) {
                    x <<- y
                    i <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) i <<- inverse
            getinverse <- function() i
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


##Return matrix that is inverse of x. Will compute inverse if not already computed. Else will return computed inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            i <- x$getinverse()
            if(!is.null(i)) {
                    message("getting cached data")
                    return(i)
            }
            data <- x$get()
            i <- solve(data, ...)
            x$setinverse(i)
            i
}
