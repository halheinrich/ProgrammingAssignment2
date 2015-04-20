## These functions work together to cache a solve() solution for a matrix
## m1 <- matrix(sample(1:100), 10, 10)
## mcm1 <- makeCacheMatrix(m1)
## im <- cacheSolve(mcm1)

## makeCacheMatrix creates a cacheable matrix for use by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve uses the cached solve if available. If not, one is created
## for use next time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached solve")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

