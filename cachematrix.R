##Function that creates a matrix that can catche its inverse

makeCatcheMatrix<-function(x=matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
  
}




catcheSolve<-function(x,...){
  #Return a matrix that is an inverse of x
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if new matrix, compute the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
  
}
