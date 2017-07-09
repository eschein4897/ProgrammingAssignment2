
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() ## calls getinverse() function  and assigns the inverse of the matrix stored in the cache to variable inv
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv) ##checks if inv is NULL or not, and returns the inverse if it exists
        }
        data <- x$get() ## create the local variable data and assigns it to the get() function, which is the value of x
        inv <- solve(data) ##assigns to inverse of local variable data
        x$setinverse(inv) ## calls setinverse() and passes the inverse of the matrix (inv) to it as an argument.. setinverse() assigns the argument passed to it to inv but in the makeCacheMatrix scope
        inv
}





















