## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse, which 
## is really a list containing a function to:
##      (1) set the value of the matrix
##      (2) get the value of the matrix
##      (3) set the value of the inverse
##      (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        setm<- function(y){
                x<<-y
                m<-NULL
        }
        getm<- function() x
        setinversem<- function(inverse)
                m<<-inverse
        getinversem<- function() m
        list(setm=setm, getm=getm, setinversem=setinversem, 
             getinversem=getinversem)
}


## Function makeCacheMatrix computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        m<-x$getinversem()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        inmatrix<- x$getm()
        m<-solve(inmatrix, ...)
        m
        ## Return a matrix that is the inverse of 'x'
}
