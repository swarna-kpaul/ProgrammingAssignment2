## Functions to create a special matrix type which will store the original matrix and the 
## inverse of the matrix. While solving the inverse of a given matrix, either already stored 
## inverse matrix in the cache will be displayed or inverse of the matrix will be calculated 
## stored in the cache

## Function to create a special matrix type. It will return a list of functions.
## set --> to set a matrix
## get --> to get a matrix
## setinverse --> set the inverse of a matrix
## getinverse --> get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
InvMat <- NULL
  set <- function(y) {
    x <<- y
    InvMat <<- NULL
  }
  get <- function() x
  setinverse <- function(inversemat) InvMat <<- inversemat
  getinverse <- function() InvMat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function to calculate inverse of a matrix and store it in cache
## If inverse of a matrix is alreadt available in the cache then it
## fetch the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
