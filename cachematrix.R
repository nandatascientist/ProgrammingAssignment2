## Following set of functions will together calculate and cache 
## the inverse of a matrix in order to avoid recomputing the 
## matrix inverse each time it is needed

## This function returns a "special" matrix that serves 
## the purpose of cache by using its  lexical scope
makeCacheMatrix <- function(x = matrix()) {

        matrixInverse<-NULL # initialize base value
        
        # function sets the value of the matrix x in cache
        set<- function(y){
                
                x<<-y  
                matrixInverse<-NULL
                
        }
        
        #function retrives the value of matrix x in cache
        get<-function()x
        
        # function sets the value of matrixInverse (for matrix x)
        setmatrixinv<-function(minv){
                
                matrixInverse<<-minv
        }
        
        # function gets the value of matrixInverse (for matrix x)
        getmatrixinv<function()matrixInv
        
        # just like with the vector means example, we return a list of the get 
        # and set methods
        list(set=set,get=get,setmatrixinv=setmatrixinv,getmatrixinv=getmatrixniv)        
}


## This function returns the inverse of the matrix x created through the 
## special function above. The inverse is retrieved from cache if it exists
## and if it does not, the inverse is computed, stored in cache, and returned

cacheSolve <- function(x, ...) {
        
        # Few Notes to consider:
        # (a) x is a "special matrix" created through above function much like special vector in example
        # (b)"..." will be the arguments passed to the inverse function again like before
        # (c) we will assume that x is a invertible (square) matrix
        
        
        # retrieve value of inverse from cache
        minv<-x$getmatrixinv
        
        # if value is not null, return as output of current function
        if(!is.null(minv)){
                
                message("getting cached data")
                return(minv)
        }
        
        # if the code gets to this point, it means that the inverse value is null
        # So (a) get the value of the matrix, (b) compute inverse, (c)update cache
        # and (d)return computed inverse
        
        workingmatrix<-x$get() 
        computedinv<-solve(workingmatrix,...) # solve(A) returns A's inverse
        x$setmatrixinv(computedinv)
        computedinv
        
        
}


## The program that will need the matrix inverse computed will first
## call makeCacheMatrix with the actual matrix for which inverse is desired.
## makeCacheMatrix will return a function object as described above. 
## The calling program  will then invoke cachesolve with the output of 
## cacheSolve (i.e. with the function object)

