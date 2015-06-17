# Takes a makeCacheMatrix object x and checks if it has a cached
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