## Put comments here that give an overall description of what your
## functions do
## R-programming assignment#coursera
## Submitted by: Thapa5

## Write a short comment describing this function
## I am using the input "a" as a matrix and set the value as m as null

## makecachematrix function is important that creates a special matrix object that can cache its inverse for the input which is an invertible square matrix
## Library(Mass) was used to calculate the inverse function
makecacheMatrix <-function(x=matrix()) { #this function defines the argument with default mode of "matrix"
  inv <- NULL #it holds the value of the assigned inverse matrix
  set <- function(y) {# it define the new make to the function as "set"
    a <<-  y #this indicates the matrix that is in the parent environment 
    inv <<- NULL # since there is a new matrix, reset inv to the NULL
  }
  get <- function() {x} # this function help us to return the matrix argument
  setinv <- function(inverse) {inv <<- inverse} #we again assigns the value of inv in parent enviroment
  getinv <- function() {inv} # this function can help to get the value when we called
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}




## Write a short comment describing this function
##cacheinverse is a function which computes the inverse of special matrix that is returned by the makecachematrix that is written above.
## Also if the inv value as we assigned is not changed  and we already have calculated it the assigned cacheSolve will retrieve the inverse function

cachesolve <- function(x,...){
  inv<- x$getinv ()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
    
  }
  mat<- x$get()
  inv <- solve(mat,...)
  x$setinv(inv)
  inv
  
}
#I used the following expression to test my function
a_matrix <- makecacheMatrix(matrix(1:14, nrow=2, ncol=2))
a_matrix$get()
a_matrix$getinv()
cachesolve(a_matrix)
a_matrix$getinv()

