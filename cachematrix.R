## The two function return an inverse of a matrix which is cached, or
## compute the inverse if it is not cached.

## The `makeCacheMatrix` creates a list of four elements which are functions.
## The functions set a matrix, get a matrix, set the value of solve (inverse),
## And get the value of solve.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## The `cacheSolve` determines whether the matrix inverse exists (is cached)
## And returns it; Or, it computes it and stores it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)){
    message('getting cached data')
    return(s)
  }
  message('caching data and printing')
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  return(s)
}