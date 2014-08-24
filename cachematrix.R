## These two functions create an object storing a matrix and its inverse
## and providing four functions to manipulate and access them.

## makeCacheMatrix :
## Return a list of four functions acting on a single supplied matrix.
## x$get() will return that matrix, x$set() will change the matrix
## the function applies to, x$setInverse() caches a value (in this 
## case the function inverse) and x$getInverse() returns that value on 
## request.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function () x
  setInverse <- function(solve) m <<- solve
  getInverse <- function () m
  list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


## If calling the getInverse() function of the argument object shows a 
## precalculated inverse, then return it. Otherwise (m being null) use 
## solve() to find the inverse and then setInverse() to cache it, and return its
## value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
