## makeCacheMatrix is a list of functions
## used to store the inverse of a matrix in
## cache, to avoid recomputing it all the time. 


makeCacheMatrix <- function(x = numeric()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInv <- function(solve) I <<- solve
  getInv <- function() I
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve is used to check if the inverse of the matrix
## is stored in cache.  If it is, it retrieves it.  If it is not,
## it uses the setInv in the makeCacheMatrix function to cache the result



cacheSolve <- function(x, ...) {
  I <- x$getInv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I) 
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInv(I)
  I
}
