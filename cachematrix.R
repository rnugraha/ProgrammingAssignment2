## This function creates a special "matrix" object that can cache its inverse, 
## which is really a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the solve
## get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL ## initial value of solve
  ## Set value of matrix and reset solve to null
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ## Get value of matrix
    get <- function() x
  ## Set value of solve    
  setsolve <- function(solve) s <<- solve
  ## Get value of solve  
  getsolve <- function() s
  ## return list of all functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Get solve value from matrix x   
  s<-x$getsolve()
  ## If matrix x already has cached solve, return it
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ## Assign matrix x value to data
  data <- x$get()
  ## Calculate solve of data  
  s <- solve(t(data))
  ## Set result to matrix x  
  x$setsolve(s)
  ## return solve
  s
}
