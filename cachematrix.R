
## This first function, makeCacheMatrix, creates a special matrix, 
## which actually returns a list that contains a function 
## which allows you to: 
##set the value of the matrix (set), 
## get the value of the matrix (get), 
## set the value of its inverse (getinv), and 
## get the value of the inverse of the matrix.

## The argument of the function is a matrix - a square invertible one. If the matrix 
## is of higher (than 2x2) dimension, then once should replace solve() by ginv() 
## (function ginv() requires package MASS).


makeCacheMatrix <- function(x = matrix()) {
                    inv <- NULL
                    set <- function(y){
                        x <<- y
                        inv <<- NULL
                    }
                    get <- function() x
                    setinverse <-function(inverse) inv <<- inverse
                    getinverse <- function() inv
                    list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## The following function calculates the inverse of the special matrix created
## in makeCacheMatrix. First, cacheSolve checks if the inverse has been calculated,
## if so, it returns the previously calculated inverse (inv).
## Otherwise, it calculates 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
