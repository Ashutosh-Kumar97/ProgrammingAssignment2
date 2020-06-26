## the first function in the code creates a matrix and stores its inverse in the cache 
## the second function creates the inverse of the matrix 
 
## the makeCacheMatrix function creates a matrix and also stores its inverse in the cache using the setInverse function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){x}
        setInverse <- function( inverse ) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function creates the inverse of the input matrix or outputs the inverse if it is already cached 

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
