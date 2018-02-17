##cachematrix.R
##MRR 20180217

## The follow pair of functions are submitted to satisfy the requirements of 
## the programming assigment for Week 3 of R Programming
## The goal of the assignment is to demostrate lexical scoping rules in the
## R programming environment.
## To work correctly the functions must be executed in succession, first the 
## makeCaceMatrix function is called to establish the environment for the 
## "matix" object, then the cacheSolve function is called to create the inverse
## of the "matrix".  Steps to execute are as follows... 

## create a matix as per the forum notes
## m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

## using the makeCacheMatrix function create a matix object
## myMatrix_object <- makeCache_matrix(m1)

## using the cacheSolve function, create the inverse of the provided matrix
## cacheSolve(myMatrix_object)

## Once the inverse of the matrix has been solved it is cached for later use
## a second call to cacheSolve(myMatrix_object) will retrieve the result from
## the cache, within the matrix object's environment.

## other notes...
## executing myMatrix_object$get() will disply the original matrix
## executing myMatrix_object$set(matrix) will replace the existing matrix
##      new matrix and set the cached inverse to 'null'
## executing myMatrix_object$getinverse() will display the cached inverse matrix
##      if the inverse function has not been created it will display null
## executing myMatrix_object$setinverse(matrix) simply stores the matrix
##      provided into the matrix object's envrironment it does not calculate
##      the inverse that is the function of the 'cacheSolve' function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        #setinverse <- function(beavis) i <<- beavis
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        
        
        i
}
