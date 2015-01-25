## Author: pulkit-goel

## R script creates an understanding of the concept of caching and how lexical scoping can be used to prevent
## unauthorised access to data.
## makeCacheMatrix() is used for creating a user defined data structure which stores a matrix and inverse of matrix once 
## computed. makeCacheMatrix() returns a list containing functions :
## 1) set
## 2) get
## 3) setinv
## 4) getinv
## makeCacheMatrix() stores the matrix(x) and its inverse(inv) within its lexical scope and x and inv cannot
## be directly modified from outside the function. 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                              ## stores the inverse the matrix when computed first
  set <- function(y) {                     ## set() function is used for altering the value of x from outside the scope of 
    x <<- y                                ## makeCacheMatrix() using "<<-" operator
    inv <<- NULL                           ## IMP: when set() is called x is modified we need to reset the value of inv back to NULL
  }
  get <- function() x                          ## returns the value of x
  setinv <- function(inverse) inv <<- inverse  ## set the value of inv equal to inverse
  getinv <- function() inv                     ## returns value of inv
  list(set = set, get = get,                   ## returns the list containing the above four functions
       setinv = setinv,
       getinv = getinv)
}
## cacheSolve() takes the the return list of makeCacheMatrix() as input returns the inverse of the matrix strored in x either
##  by computing or returning the cached value of inverse.
cacheSolve <- function(x, ...) {
          inv <- x$getinv()          ## calls getinv() of x to store the value of inverse in variable inv.
  if(!is.null(inv)) {                ## if inv is NOT NULL implies the inverse has been calculated before therefore return the
    message("getting cached data")   ## cached value of the inverse
    return(inv)                     
  }
  data <- x$get()                    ## else store the value of the matrix in data
  inv <- solve(data, ...)            ## compute the inverse of matrix stored in data and return inv
  x$setinv(inv)
  inv
}
