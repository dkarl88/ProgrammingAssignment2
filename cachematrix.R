## functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the inverse property
        i<-NULL
        
        ## set the Matrix
        set<-function( y ){
                m<<-y
                i<<-NULL
        }
        
        ## get the matrix
        get<-function(){
                
        ## Return the matrix        
                m
        }
        
        ## set the inverse of the matrix
        setinverse<-function(inverse){
                i<<-inverse
        }
        
        ## set the inverse of the matrix
        getinverse<-function(){
                
        ## Return the inverse propert
                i
        }
        
        ## Return a list of the methods
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Calc the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calc then the "cachesolve" should 
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## Return the inverse if its already calc
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse
        m <- solve(data) %*% data
        
        ## Set the inverse to the object
        x$setinverse(m)
        
        
        ## Return the Matrix
        m
}