## Assumption x is a square invertible matrix and is nonsingular


# makeCacheMatrix - This function creates a special "matrix" object that
# can cache its inverse
# returns a list containing functions to
#        1. set the matrix
#        2. get the matrix
#        3. set the inverse
#        4. get the inverse
# this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
#   set the matrix
    set <- function(y){
      x <<- y
      m <<- NULL
    }
#   get the matrix
    get<- function() x
    
#   set the inverse of matrix    
    setsolve <- function(solve) m <<- solve
    
#   get the inverse of matrix
    getsolve <- function() m
    
    list(set =  set, get = get, setsolve = setsolve, getsolve = getsolve)
}


# cacheSolve - This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {

# output of makeCacheMatrix is used as an inut to this function     
# This function returns a matrix that is the inverse of 'x'
    m <- x$getsolve()
    
    #check if the inverse has already been calculated earlier
    if (!is.null(m)){
      message("getting cached data")
      return(m)
    }
    
    # If the inverse is not available, calculate the inverse 
    data <- x$get()
    m <- solve(data, ...)
    
    #set the value of the inverse in the cache
    x$setsolve(m)
    
    return(m)
}
