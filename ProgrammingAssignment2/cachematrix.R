## functions do

##This function caches inverted matrix.
makeCacheMatrix <- function(invx = matrix()) {
  
  inverseMatrix <- NULL
  set <- function(y) {
    invx <<- y
    inverseMatrix <<- NULL
  }
  get <- function() invx
  setinverse <- function(ginv) inverseMatrix <<- ginv
  getinverse <- function() inverseMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


##This methos compute inverse of "matrix" usgin makeCacheMatrix.

cacheSolve <- function(invx, ...) {
  ## Return a matrix that is the inverse of 'invx'
  
  inverseMatrix <- invx$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- invx$get()
  inverseMatrix <- mean(data, ...)
  invx$setmean(inverseMatrix)
  return(inverseMatrix)
}

