## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        inverse_ <-NULL
        set<-function(y){
                x<<-y
                inverse_<<-NULL
                
        }
        get<-function(){x}
        set_inverse()<-function(inverse){inverse_<<-inverse}
        get_inverse()<-function(){inverse_}
        
        list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
        
}


## Write a short comment describing this function


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
