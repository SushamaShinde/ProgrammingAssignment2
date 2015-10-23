# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.


##The first function, makeVector creates a special "vector", which is really a list containing a function to
##1. set the value of the vector
##2. get the value of the vector
##3. set the value of the mean
##4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## Output of makeCacheMatrix AND cacheSolve functions as follows:
## x = rbind(c(2, 4), c(4, 2))
## m = makeCacheMatrix(x)
## m$get()
##            [,1] [,2]
##      [1,]    2    4
##      [2,]    4    2
## cacheSolve(m)
##              [,1]       [,2]
##      [1,] -0.1666667  0.3333333
##      [2,]  0.3333333 -0.1666667
