cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

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

# Sample invertible matrices
# Define the matrix in R
A <- matrix(c(1, 2,
              3, 4),
            nrow = 2,  # number of rows
            byrow = TRUE)  # fill the matrix by rows

# Print the matrix
print(A)

# Define the 3x3 matrix
B <- matrix(c(2, 1, 3,
              1, 0, 2,
              4, 5, 6),
            nrow = 3, 
            byrow = TRUE)

# Define the 4x4 matrix
C <- matrix(c(1, 2, 3, 4,
              0, 5, 6, 7,
              0, 0, 8, 9,
              0, 0, 0, 2),
            nrow = 4,
            byrow = TRUE)

rm(list = ls())