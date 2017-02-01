# PROGRAMMING ASSIGNEMENT
#
# Your assignment is to write a pair of functions that cache the inverse of a 
# matrix.
# 
# Write the following functions:
# 
# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.
# 
# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache.

# START
# makeCacheMatrix: This function defines a matrix object and its following 
# functions: *set*, *get*, *setinv*, *getinv*. The *set* and *setinv* functions
# make use of the <-- assignement operator to alocate the variables in its
# parent environment.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # Defines the inverse as being NULL, but only in the constructed environment.
  set <- function(y) {
    x <<- y # Defines the matrix to be stored
    i <<- NULL # Defines the inverse as being NULL at the parent environment.
  }
  get <- function() x # Returns the matrix stored
  setinv <- function(inv) i <<- inv # Defines the inverse of the matrix to be stored.
  getinv <- function() i # Returns the inverse of the matrix stored.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) # Final list of functions representing the matrix object.
}


## cacheSolve: This function checks whether the inverse of a given matrix object
## is already calculated. If so, it retrieves the cached data. If not, it 
## calculates the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv() # This line calls the previously defined object function to get the inverse of the matrix
  if(!is.null(i)) { # Checks whether the inverse is already calculated, and if so, returns it.
    message("getting cached data")
    return(i)
  }
  data <- x$get() # If not, the original matrix is recovered, and its inverse is calculated.
  i <- solve(a = data, ...)
  x$setinv(i) # The inverse is stored in the matrix object through the setinv function
  i
}

# END

# TESTS: tm stands for Test Matrix

# Test 1
tm_1 <- matrix(data = 1:4 , nrow = 2, ncol = 2)
tm_1
tm_1_cache <- makeCacheMatrix(tm_1)
cacheSolve(tm_1_cache)
cacheSolve(tm_1_cache)
solve(tm_1)

# Test 2
tm_2 <- matrix(data = c(5, 9, 11, 20) , nrow = 2, ncol = 2)
tm_2
tm_2_cache <- makeCacheMatrix(tm_2)
cacheSolve(tm_2_cache)
cacheSolve(tm_2_cache)
solve(tm_2)


# Test 3
tm_3 <- matrix(data = c(5, 10, 11, 20) , nrow = 2, ncol = 2)
tm_3
tm_3_cache <- makeCacheMatrix(tm_3)
cacheSolve(tm_3_cache)
cacheSolve(tm_3_cache)
solve(tm_3)

