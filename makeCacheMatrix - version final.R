## version 2, change mean for inverse
## change x=numeric() for x=matrix()
## m is the inverse now. "inversa" is in Spanish
## the operator <-- save inverse in the parent environment, so
##   the inverse value is safe
## solve is the formula of inverse of an matrix
## the logic of the program is the same of makeVector

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversa <- function(inversa) m <<- inversa
  getinversa <- function() m
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}

cacheSolve <- function(x, ...) {
  m <- x$getinversa()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversa(m)
  m
}

###example
mv<-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
mv$get()
mv$getinversa()
cacheSolve(mv)

