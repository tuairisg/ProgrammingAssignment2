## These functions demonstrate caching a matrix,
## and calculating and caching its inverse

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse
##
## Sample call: 
## myMatrix = matrix(c(4,3,3,2),ncol=2,nrow=2,byrow=TRUE)
## cachedMatrix = makeCacheMatrix(myMatrix)
## cachedMatrix$get()
## [,1] [,2]
## [1,]    4    3
## [2,]    3    2

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
##
## Sample call: 
## myMatrix = matrix(c(4,3,3,2),ncol=2,nrow=2,byrow=TRUE)
## cachedMatrix = makeCacheMatrix(myMatrix)
## cacheSolve(cachedMatrix)
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## To access the newly cached inverse:
## cachedMatrix$getinverse()
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
