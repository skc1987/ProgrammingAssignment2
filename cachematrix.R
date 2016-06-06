## The functions work in tandem and help save time while performing matrix calculations with a large data set. The developed functions cache the inverse of the matrix and can obtain the inverse from cache.

## makeCacheMatrix takes an invertible square matrix and makes an object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  ### x should be a non-singular, square matrix
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<- inverse
  getinverse<-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function computes the inverse of the object returned by makeCacheMatrix. If the inverse has already been calculated, then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  m1<-x$get()
  m<-solve(m1, ...)
  x$setinverse(m)
  m
}

