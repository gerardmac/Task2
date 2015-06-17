# Takes a matrix and caches it in x.  x can then be accessed 
# through the returned list with get() or changed with set().  
# getInv() and setInv() are used to get and set x's inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(invM) inv <<- invM
  getInv <- function() inv
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}