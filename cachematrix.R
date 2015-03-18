## CACHING THE INVESRE OF MATRIX
## *********************************************************************************
## If we need to use repeatedly the inverse of a matrix, we can use the following
## 2 functions: the first creates a special list that can cache its inverse, and the 
## second one retrieve it.
## *********************************************************************************


## The first function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse. This function return a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setsolve<-function(solve) m<<-solve
        getsolve<-function () m
        list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setsolve(m)
        m
}
