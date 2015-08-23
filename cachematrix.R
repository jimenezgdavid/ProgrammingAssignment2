## These functions allow the client to cache the
## value of a matrix and it's iverse. They also
## provide a way to get and set the matrix and 
## it's inverse as well as solve the last one.

## This function returns a list of functions 
## containing a matrix "x" given by the user (or   
## by default an empty matrix) and it's  
## inverse. It also provides several functions 
## to get/set the value of such matrices.

makeCacheMatrix <- function(x = matrix()) {
     i<-NULL
     set<- function(y){
          x<<-y
          i<<-NULL
     }
     get<- function() x
     setinverse<- function(inverse) i<<-inverse
     getinverse<- function() i
     list(set=set, get= get, setinverse=setinverse,getinverse=getinverse)
}


## This function solves the inverse function of
## a list  of functions "x" such as created by 
## makeCacheMatrix if  it hasn't been solved
## yet; otherwise, it returns the inverse cached 
## in x.

cacheSolve <- function(x, ...) {
     i<-x$getinverse()
     if (!is.null(i)){
          message("getting cached data")
          return(i)
     }
     data<- x$get()
     i<- solve(data,...)
     x$setinverse(i)
     i
}
