## These functions will calculate the inverse of a matrix


## makeCahceMatrix will create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
        x<<-y
        m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)m<<-inverse
  getinverse<-function()m
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve will compute the inverse of the special "matrix" returned by makeCacheMatrix
## if the inverse has already been calculated (and the matrix is unchanged), then cacheSolve
## should retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
      m<-x$getinverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data<-x$get()
      m<-solve(data,...)
      x$setinverse(m)
      m
        
}
