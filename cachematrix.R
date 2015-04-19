## The purpose of these functions is to efficiently invert a matrix
##   For the first execution of the function, the calculation will be done.
##   For subsequent executions, a cached value for the matrix will be used.  To
##   handle the cacheing, lexical scoping is taken advantage of.

## makeCacheMatrix constructs a list of functions.  The functions are as follows:
##    set - takes a matrix and saves it to the parent environment (cacheSolve)
##    get - retrieves the original matrix
##    setInverse - saves the inverted matrix to the parent environment (cacheSolve)
##    getInverse - retrieves the inverted matrix.
makeCacheMatrix <- function(x = matrix()) {
  InvMatrix <- NULL
  set <- function(y) {
    x <<- y
    InvMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) InvMatrix <<- inverse
  getInverse <- function() InvMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## List created with makeCacheMatrix is passed to this function.  If the inverted
###   matrix is already calculated, that value is returned.  If it is not, the 
###   solve function is used to calculate it.  Functions in makeCacheMatrix are used
###   to get and retrieve the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        InvMatrix <- x$getInverse()
        if(!is.null(InvMatrix)) {
              message("getting cached data")
              return(InvMatrix)
        }
        OrigMatrix <- x$get()
        InvMatrix <- solve(OrigMatrix)
        x$setInverse(InvMatrix)
        InvMatrix
          
}
