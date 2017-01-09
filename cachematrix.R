## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly
## makeCacheMatrix and cacheSolve are two functions that are used to create 
## special object that stores a matrix and caches its inverse


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix<- NULL
  set <- function(q) {
    p <<- q
    inv_matrix <<- NULL
  }
  get <- function() p
  setInverse <- function(inverse) inv_matrix <<- inverse
  getInverse <- function() inv_matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated ,then it 
## should retrieve the inverse from the cache provided that the matrix has not
## changed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_matrix <- p$getInverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  mat <- p$get()
  inv_matrix <- solve(mat,...) 
  p$setInverse(inv_matrix)
  inv_matrix  
}
