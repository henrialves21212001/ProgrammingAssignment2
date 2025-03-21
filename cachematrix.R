## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## inv is a variable to store the cached inverse of the matrix
## it is initially empty, the set function update x matrix to y 
## a new matrix, x <<- y updates the stored matrix, and
## inv <<- NULL ensures that if x changes it must be recalculated
## get allows external functions to access the matrix
## setInverse stores the inverse in inv and retrieve it when
## needed; getInverse stands for If the inverse has already 
## been calculated, it can be retrieved instead of recomputing it;
## the list prepare the next step the cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Variable to store the cached inverse
  
  set <- function(y) {
    x <<- y  # Assign a new matrix
    inv <<- NULL  # Reset cached inverse (since matrix changed)
  }
  
  get <- function() x  # Return the stored matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Cache inverse
  getInverse <- function() inv  # Return cached inverse (if available)
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
## Firstly to see if the inverse is already cached
## x$getInverse() tries to retrieve the cached inverse.
## Then, If inv is not NULL, it means the inverse is already computed and 
## stored. The message("getting cached data") prints a message indicating that 
## the cached inverse is being used. And return(inv) returns the cached inverse 
## immediately, avoiding recomputation. The variable mat
## compute the inverse; inv calculated the inverse of mat
## Then  x$setInverse(inv), which stores the computed inverse in the cache.
## Finally, return(inv) the inverse is returned

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if inverse is already cached
  
  if (!is.null(inv)) {
    message("Getting cached inverse")  
    return(inv)  # Return cached inverse
  }
  
  mat <- x$get()  # Retrieve the matrix
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the inverse
}
