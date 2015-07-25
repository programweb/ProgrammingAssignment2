## Getting an inverse matrix can be consuming of system resources. 
## These functions help minimize that by caching previous results 
## as needed.

# example test
# 1]    m <- matrix(1:4, 2, 2)
# 2]    t <-makeCacheMatrix()
# 3]    t$set(m)
# 4]    cacheSolve(t)
# 5]    cacheSolve(t)    # see message that it is from cache here

# makeCacheMatrix has 4 nested functions allowing setting and getting of a matrix or inverse
makeCacheMatrix <- function( x = matrix() ){
  im <- NULL
  set <- function(y){
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setIM <- function( imtrx ) im <<- imtrx
  getIM <- function() im
  list(set = set, get = get, setIM = setIM, getIM = getIM)
}

# cacheSolve takes a matrix and returns the inverse (possibly from cache)
cacheSolve <- function(x, ...){
  cachedInvX <- x$getIM()
  if( ! is.null(cachedInvX)){
    message("getting cached data")
    return(cachedInvX)
  }
  data <- x$get()
  cachedInvX <- solve(data) %*% data
  x$setIM( cachedInvX )
  return( cachedInvX )
}