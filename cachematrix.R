## These two functions calculate and cache the inverse of a matrix
## and check to see whether the inverse of a given matrix has
## been calculated and cached before; if so, the cached
## inverse is retrieved. If not, the inverse is calculated
## and cached for future retrieval.

## Returns funtions which can be called to pass (set) and 
## retrieve (get) to-be-inverted matrices, to store calculated
## matrix inverses to cache (setinv), and to check the cache for
## stored inverses and retrieve them (getinv).

makeCacheMatrix <- function(x = matrix()) {
      # Returns four functions that can be called to check, store
      # to, and retrieve from the cache.
      
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calls the "get" or "getinv" functions defined in makeCacheMatrix 
## to either retrieve a cached matrix's inverse, or solve the inverse
## of a matrix that isn't in the cache yet.

cacheSolve <- function(x, ...) {
      # Returns a matrix that is the inverse of the matrix 'x'
      
      inv <- x$getinv()
      if(!is.null(inv)){
            message("getting cached inverse")
            return(inv) #returns the cached inverse
      }
      data <- x$get()
      inv <- solve(data,...)
      x$setinv(inv)
      inv
}
