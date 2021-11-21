## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates a special matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
set<-function(y){
  x<<-y
  inv<<-NULL
}
get<-function()x
setinv<-function(inverse)inv<<-inverse
getinv<-function()inv
list(set=set,get=get, setinv=setinv,getinv=getinv)
}



## Write a short comment describing this function
##cachesolve is function that can compute the inverse of the special matrix
## if the inverse is already calculated , it can be return by makeCacheMatrix written above
## also if the matrix has not been changed, the function cachesolve can retrive from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data()<- x$get()
  inv<- solve(data(),...)
  x$setinverse(inv)
  inv
}

