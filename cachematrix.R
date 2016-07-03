## R Programming - Week 2 Project Assignment


## The functions below were created in order to complete the Project Assignment
## for the R Programming course.
## Since matrix inversion is usually a costly computation, the focus of this
## assignment is to find a way to cache the inverse of a matrix rather than
## compute it repeatedly.



## makeCacheMatriz creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) s <<- solve
     getinverse <- function() s
     list( set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
     
}

## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix ## has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     s <- x$getinverse()
     if(!is.null(s)){
          message("getting cached data")
          return(s)
     }
     data <- x$get()
     s <- solve(data, ...)
     x$setinverse(s)
     s
     ## Return a matrix that is the inverse of 'x'
}