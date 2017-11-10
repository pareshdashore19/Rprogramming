## The first function, `makeCacheMatrix` creates a special "matrix", which is
##  really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse oa matrix
##4.  get the value of the inverse of matrix



makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
                     x <<- y
                    m <<- NULL
                 }
           get <- function() x
           setsolve <- function(solve) m <<- solve
           getsolve <- function() m
           list(set = set, get = get,
                           setsolve = setsolve,
                           getsolve = getsolve)

}


## 
## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse of matrix has already been calculated. If so, it `get`s the inverse of matrix from the
## cache and skips the computation. Otherwise, it calculates the inverse of matrix of
## the data and sets the value of the inverse of matrix in the cache via the `setsolve`
## function.

cacheSolve <- function(x, ...) {
        
      m <- x$getsolve()
           if(!is.null(m)) {
                     message("getting cached data")
                     return(m)
                 }
           data <- x$get()
           m <- solve(data, ...)
           x$setsolve(m)
          m
}
