## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        InvMatrix <- x$getInverse()
        message("Step 1")
        if(!is.null(InvMatrix)) {
              message("getting cached data")
              return(InvMatrix)
        }
        message("Step 2")
        OrigMatrix <- x$get()
        InvMatrix <- solve(OrigMatrix)
        x$setInverse(InvMatrix)
        InvMatrix
          
}
