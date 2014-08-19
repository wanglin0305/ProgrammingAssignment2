## Caching the Inverse of a Matrix

##this function is to create a matrix,and it can set and get a matrix,and caching
## its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set=set,get=get,setSolve=setSolve,getSolve=getSolve)
}


## this function calculates the inverse of the matrix created with the above function
## if the inverse has been calculated,it gets the value frome the cache and skips the
## computaion.Otherwise,it calculates the inverse of the data via the setSolve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setSolve(s)
        s
}
