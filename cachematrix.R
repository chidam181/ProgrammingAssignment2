## Caching the inverse of a Matrix
## Matrix inversion is computationally expensive to be performed repeatedly. 
## Therefore the functions in this program calculates the inverse and stores in 
## the cache. This data is accessed whenever the inverse is called. 

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        mat1 <- NULL
        set <- function(y){
                x<<-y
                mat1 <<- NULL
        }
        get <- function() x
        setInverse <- function(mat1) mat1 <<- inverse
        getInverse <- function() mat1
        list(set=set, 
             get=get, 
             setInverse=setInverse, 
             getInverse=getInverse)
                
}


## The output from this function is the inverse of the matrix. Two steps are
## followed here, the first step checks if the inverse already exists, if it 
## does, it returns the value, else it will calculate the value using setinverse 
## function

cacheSolve <- function(x, ...) {                
        mat1 <- x$getInverse()                   ## Check the cache
        if (!is.null(mat1)) {                    ## If cache exists
                message("obtaining cache data") ## Message indicating status
                return(mat1)                     ## Returns the cache data
        }
        mat <- x$get()          ## Obtaining data fro mother function
        mat1 <- solve(mat, ...)  ## Calcuating inverse
        x$setInverse(mat1)       ## Store data of new matrix
        mat1  ## Return a matrix that is the inverse of 'x'
}
