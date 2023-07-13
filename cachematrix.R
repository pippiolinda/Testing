## These functions should make faster the inversion of a set of matrices. 
## The idea is to save the inverse of a matrix in the cache and, when needed, check if the inverse already exists before calculating it.
## If yes, the function recovers the inverse. If no, it computes it.

## makeCacheMatrix creates a matrix that is a list containing a function able to set and get the starting matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
        set<-function(y) {
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinv<-function(solve) m<<-solve
        getinv<-function() m
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve takes a matrix as input and computes its inverse only if the latter isn't already in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
