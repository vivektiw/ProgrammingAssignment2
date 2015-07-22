## This function creates a special "matrix" object that can cache its inverse.(Vivek)

makeCacheMatrix <- function(x = matrix()) {v <- NULL
                                           set <- function(y) {
                                             x <<- y
                                             v <<- NULL
                                           }
                                           get <- function() x
                                           setinverse <- function(inverse) v <<- inverse
                                           getinverse <- function() v
                                           list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {v <- x$getinverse()
                                if(!is.null(v)) {
                                  message("cached data.")
                                  return(v)
                                }
                                data <- x$get()
                                v <- solve(data)
                                x$setinverse(v)
                                v
        ## Return a matrix that is the inverse of 'x'
}
