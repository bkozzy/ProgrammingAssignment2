## Put comments here that give an overall description of what your
## functions do

##function takes a matrix as an argument.
##The function returns a list of functions that operates on the matrix x
##The functions are
###get - return them matrix
###set - sets the matrix and null as inv (cached inverted matrix)
###setinv - sets the inverted matrix 
###getinv - ges the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } 
  get <- function() x 
  setinv <- function(inv_m) inv <<- inv_m
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

##This function gets the inverted matrix for the 'special' matrix defined in makeCacheMatrix
##It takes matrix x as a parameter
##It return inverted matrix
##If the inverted matrix is already stored in 'inv' it return the cached matrix
##if the inverted matrix hasn't been computed yet it takes x as gets inverted matrix on it and later sets it
## to the 'inv' field.

##The code assummes that x is revertible 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get() 
        inv <- solve(data, ...)
        x$setinv(inv) 
        inv
        
}

#Example
#m <- matrix(c(1,0,5,2,1,6,3,4,0), 3,3)
#inv_m <- makeCacheMatrix(m)
#cacheSolve(inv_m)   (to compute)
#cacheSolve(inv_m)    (to retrieve from the cache)
