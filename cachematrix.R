## Two functions that work together in order to cache inverse matrix calculations 

## First functin creates a new object (Cachematrix) with methods for getting / setting the matrix and getting 
## and setting the inverse. This function handles the cache and the initialization of the cache when a new matrix 
## is loaded.

makeCacheMatrix <- function(x = matrix()) {
        
        invm <- NULL ##initialization
        
        #define 4 function setting and getting the matrix and setting and getting the inverse
        setmatrix <- function (y){
                x <<-y
                invm <<- NULL
        }
        getmatrix <- function() x
        setinvmatrix <- function(i) invm <<- i
        getinvmatrix <- function() invm
        
        #return the object list
        list(getmatrix = getmatrix,setmatrix = setmatrix,
             setinvmatrix = setinvmatrix,getinvmatrix = getinvmatrix)
}


## This function calculates the inverse of an "cachematrix" object unless this inverse is previously cached.
## in this case it returns the inverse without calculations

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##looking for previously cached inverse matrix
        i <- x$getinvmatrix()
        if (!is.null(i)) {
                print("Getting cached inverse matrix")
                return(i)
        }
        
        ##get the matrix
        mymatrix <-x$getmatrix()
        ## let's calculate the inverse matrix over i
        i = solve(mymatrix)
        # save inverse matrix for cache purposes
        x$setinvmatrix(i)
        #return the inverse matrix
        i
}
