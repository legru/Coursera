## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matrix = matrix()) {
  # store for the inverse matrix
  inverse <- NULL
  # setting/getting functions
  set <- function(data) {
    matrix <<- data
    inverse <<- NULL
  }
  get <- function() matrix
  set_inverse <- function(matrix.inverse) inverse <<- matrix.inverse
  get_inverse <- function() inverse
  # return list of setters and getters
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  x.inverse <- x$get_inverse()
  if(!is.null(x.inverse)) {
    message("getting cached data")
    return(x.inverse)
  }
  # compute inverse of x
  x.matrix <- x$get()
  x.inverse <- solve(x.matrix)
  x$set_inverse(x.inverse)
  ## Return a matrix that is the inverse of 'x'
  x.inverse
}