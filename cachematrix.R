## Put comments here that give an overall description of what your
## functions do


## Below are a pair of functions that are used to create a special object that 
## stores a matirx and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        inverse_ <-NULL
        set<-function(y){
                x<<-y
                inverse_<<-NULL
                
        }
        get<-function(){x}
        set_inverse<-function(inverse){inverse_<<-inverse}
        get_inverse<-function(){inverse_}
        
        list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
        
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        XInverse<-x$get_inverse()
        
        if(!is.null(XInverse)){
                message("inversed")
                XInverse
        } 
        
        
        mat<-x$get
        XInverse<-solve(mat)
        x$set_inverse
        XInverse
        
}
