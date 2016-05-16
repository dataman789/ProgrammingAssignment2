## makeCacheMatrix creates a list containing functions to build a matrix and get or set its inverse
## cacheSolve first checks to see if the matrix inverse has already been calculated
##    if so, returns it, otherwise calculates it and stores it until the matrix setInverse function

## makeCacheMatrix returns a special matrix containing 4 functions to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  getInverse <- function() {
    inv
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## takes a special matrix made by makeCacheMatrix
## checks to see if the inverse is not null.  
## if not NULL, returns the inverse.  
## if NULL, calculates the inverse and stores it for future calls

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cache inverse")
    return(i)
  }
  
  actualMatrix <- x$get()
  i <- solve(actualMatrix)
  x$setInverse(i)
  i
}
