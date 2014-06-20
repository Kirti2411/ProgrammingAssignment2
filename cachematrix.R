# The following functions are used for Matrix inversion and the below function are used for caching the result so that the computaions
# are not done repeatedly if already done.
#The assumptions are that the input matrix is always square invertible matrix

# The makeCacheMatrix creates a storage object for the inverse of matrix.The method also has get and set functions for setting the 
# and retrieval of values from the objects.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

# The cacheSolve function creates the inverse of a square matrix.
# The solve function in R is used for calculating the inverse but the only function is only triggered if the inverse was not calculated previously

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
