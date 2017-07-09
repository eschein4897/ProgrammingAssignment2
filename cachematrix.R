
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ##initialize to NULL
        set <- function(y) { ##create matrix in working environment
                x <<- y
                inv <<- NULL
        }
        get <- function() x ##get value of matrix
        setinverse <- function(inverse) inv <<- inverse ##invert matrix and store in the cache
        getinverse <- function() inv ##get the inverted matrix from cache
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## returning the functions into the working environment
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





















