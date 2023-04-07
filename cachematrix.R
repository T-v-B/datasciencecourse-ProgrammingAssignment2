## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special "matrix" that can cache its inverse.
# For a square invertible matrix X, solve(X) returns its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInvMatrix <- function(solve) m <<- solve
    getInvMatrix <- function() m
    list(set = set, get = get, getInvMatrix = getInvMatrix, setInvMatrix = setInvMatrix)
}


## Write a short comment describing this function

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve
# should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- getInvMatrix()
  if (!is.null(m)) { # See if the matrix has been calculated before, if so, grab it's inversed value.
    message("Getting cached matrix.")
    return(m)
  }
  data <- get()
  tryCatch(
    {
        m <- solve(data, ...) # Where the inverse takes place.
    },
    error=function(cond) {
        message("Inverse Matrix does not exist.")
        message(cond)
    },
    finally={
        print(m)
    }
  )    
  setInvMatrix(m)
  m
}

# Test it out.
matt22 <- matrix(runif(1:9), nrow = 3, ncol = 3)
matt22
set(matt22)
cacheSolve(matt22) # No Cache Money
cacheSolve(matt22) # Cache Money

