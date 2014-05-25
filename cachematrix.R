## This function is used to store the value of a matrix in a local environment within the function
## This function returns a list with four functions - namely set, get, setcache and getcache
## When this function is called, 
##   without any arguments - It sets the value of x to a NULL matrix, and sets the cache_value to NULL
##  with arguments - sets the value of x to the matrix passed in the argument

makeCacheMatrix <- function(x = matrix()) {
  cache_value <- NULL
  set <- function(y) {
    x <<- y    				## Note the "<<-" operator sets the value of x in the parent environment(makeCacheMatrix) of the set function
    cache_value <<- NULL	## Note the "<<-" operator sets the value of cache_value in the parent environment(makeCacheMatrix) of the set function
  }
  
  get <- function() x
  
  setcache <- function(inv_matrix) cache_value <<- inv_matrix
  
  getcache <- function() cache_value
  
  ## Returns a list with the functions that were created
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}


## Return a matrix that is the inverse of 'x'
## checks if the cache is filled, else computes (and stores it for subsequent retrieval)

cacheSolve <- function(x, ...) {
  
  ## Check if the Inverse matrix is already available in the cache
  local_inv_matrix <- x$getcache()
  if(!is.null(local_inv_matrix)) {
    message("getting cached data")
    return(local_inv_matrix)
  }
  data <- x$get()
  
  ## Calculate the Inverse of the non-singular matrix and store it in the cache
    local_inv_matrix <- solve(data, ...)
    x$setcache(local_inv_matrix)
    local_inv_matrix
}
