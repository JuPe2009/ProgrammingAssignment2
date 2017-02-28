## This function creates a special "matrix" object that can cache its inverse.
## Saves the matrix to x and its inverse to z.

makeCacheMatrix <- function(x = matrix()) {
  
  z <- NULL

  # sets matrix and resets cached inverse
  
  set <-function(y) {
    
    x <<- y
    z <<- NULL
  }
  
  # returns matrix
  
  get <- function () x
  
  # set inverse matrix
  
  setSolve <- function(solve) z <<- solve
  
  # returns cached inverse matrix
  
  getSolve <- function() z
  
  list(set=set, get=get, setSolve = setSolve, getSolve = getSolve)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  z <- x$getSolve()
  
  if (!is.null(z)) {
    message("Devuelvo la matriz en caché")
    return(z)
  }
  
  inversematrix <- x$get()
  z <- solve(inversematrix,...)
  x$setSolve(z)
  z
}
