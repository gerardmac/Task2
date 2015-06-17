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


# Takes a list x and checks if it has a cached
# inverse matrix. If it does, the inverse matrix is returned.
# Otherwise x's cached matrix is assigned to originalMatrix 
# and the inverse matrix is created and set inside x.
# Finally the inverse matrix is returned.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {  # if x's inv has been set return it
    message("getting cached data")  
    return(inv)
  }
  originalMatrix <- x$get()   #otherwise get x's matrix
  inv <- solve(originalMatrix) ## calculate the inverse 
  x$setInv(inv)  # set the x object's inverse matrix
  inv
}