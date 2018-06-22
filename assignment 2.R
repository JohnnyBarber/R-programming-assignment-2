##The following 2 functions are used to create a special object that
##stores a matrix and cache its inverse

##Function makeCacheMatrix creates a special "matrix", which is
##really a list containing a function to:
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the inverse of the matrix
##4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##Function cacheSolve can compute the inverse of the special "matrix".
##If the inverse of the special "matrix" had been already computed, it
##will get the inverse from the cache instead of computing the inverse
##of the matrix. Otherwise, cacheSolve computes the inverse of the 
##matrix and store the inverse in the cache.

cacheSolve <- function(x, ...){
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
