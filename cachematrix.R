
##Two functions - creates an object (matrix), solves for inverse of matrix and stores previous solutions
 
##makeCacheMatrix: creates a matrix
makeCacheMatrix <- function(x = matrix()) { #create matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x  #functions for cacheSolve to access 
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,  #list of functions defined for cacheSolve to access
       s = setinverse,
       g = getinverse)
}


##cacheSolve: returns a matrix that is the inverse of the matrix created in makeCacheMatrix
##stores previous solutions

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$g()
  if(!is.null(m)) {
    message("getting cached data")  ##if solution already cached, accesses solution
    return(m) #returns cached solution
  }
  data <- x$get()
  m <- solve(data, ...) #if no solution cached, solves and caches for later access
  x$s(m)
  m #returns solution
}
