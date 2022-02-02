## this .R file will allow us to store the saved (cached) values of our calculation without 
## having to re-compute the inverse of the matrix every time we call the function.

## makeCacheMatrix allows us to set initial values for our functions, and create a cache to store
## them. If a new matrix is set, it will erase the previously saved solution so it can recalculate.

makeCacheMatrix <- function(x = matrix()) {
  
        i <- NULL                   # Creates 'i', which will store the value of the calculation. 
                                    # This is like 'm' in the example.
        set <- function(y) {
          x <<- y                   # So when the user feeds a new matrix, it sets the new matrix
                                    # to x in the parent environment (the makeCacheMatrix function)
          i <<- NULL                # clears the cache of previously saved value.
        }
        
        get <- function() x         # retrieves the value of x
        
        setinverse <- function(solve) i <<- solve         #stores solution to i
        
        getinverse <- function() i         # retrieves the value of i
        
        list(set = set, get = get,
             setinverse = setinverse,      # allows us to call functions by name in parent envir
             getinverse = getinverse)
        
}


## cacheSolve will check if i has already been solved, and if not, will calculate the new inverse.

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()         # retrieves the value of i from makeCacheMatrix environment
        
        if(!is.null(i)) {
          message("getting cached data")    # checks for a cached value of i and, if there is
          return(i)                         # a stored value, returns it 
        }
        
        data <- x$get()             # retrieves the value of x from makeCacheMatrix environment
        i <- solve(data, ...)       # calculates the inverse of the matrix
        x$setinverse(i)             # stores the result to be recalled later
        i                           # prints the result
  
}