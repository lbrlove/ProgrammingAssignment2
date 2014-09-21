## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## set() creates the matrix from supplied vector/row/column args
## get() displays/returns the matrix (if not defined, returns NULL)
## setinv() caches the inverse of the matrix (assumed valid)
## getinv() displays/returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix) {
    mat <- matrix(NA,1,1)
    inv <- matrix(NA,1,1)
    set <- function(n, row, col) {
        mat <<- matrix(n, nrow=row, ncol=col)
        inv <<- matrix(NA,1,1)
    }
    get <- function() {
        if(is.na(mat[1,1])) {
            message("No matrix set")
            return(NULL)
        }
        mat
    }
    setinv <- function() inv <<- solve(mat)
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve
## 1) fetches the inverse matrix from the proper environment
## 2) if cached, notifies and returns inverse matrix
## 3) if not cached, calculates the inverse and returns

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.na(inv[1,1])) {
        message("getting cached data")
        return(inv)
    }
    else {
        mat <- x$get()
        inv <- solve(mat)
        return(inv)
    }
}

## TEST RUN
##
## > test <- makeCacheMatrix()
## > test$get()
## No matrix set
## NULL
## > test$set(1:4,2,2)
## > test$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > test$setinv()
## > myinverse <- cacheSolve(test)
## getting cached data
## > myinverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## >
