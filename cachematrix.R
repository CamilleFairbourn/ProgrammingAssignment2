## Put comments here that give an overall description of what your
## functions do

# This function creates a list of functions that will 
# find the inverse of an invertible matrix 
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinv <- function(solve) m <<- solve
   getinv <- function() m
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## This function will either calculate the inverse of an
## invertible matrix, or it will call a cached version of
## the the inverse, thus saving the time it would take to 
## recompute it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getinv()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinv(m)
   m
}

#Here is some example code to see how it works
twoby <- matrix(c(5, -2, 2, 3), 2, 2) # Create the matrix twoby
twoby
invtwoby <- solve(twoby) #Calculate the inverse of twoby
invtwoby #see the solution
invtwoby %*% twoby #verify the solution (should give the identity matrix)

#Now do the same thing using the functions created above
a <- makeCacheMatrix(matrix(c(5, -2, 2, 3), 2, 2))
a$get() #verify that the matrix is created 
a$getinv() #verify that the value of the inverse is originally NULL
cacheSolve(a) #solve to find the inverse of the matrix
cacheSolve(a) #verify that it returns the cached inverse
