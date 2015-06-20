## The functions makeCacheMatrix and cacheSolve are used to calculate the 
## inverse of a square invertible matrix and cache the result.  The cached
## result is used in future calls to increase efficiency.

## makeCacheMatrix: 
## Uses leixical scoping to  cache a square invertible matrix 
## and its inverse.  It returns a list of functions that operate on these
## matrices.

makeCacheMatrix <- function(x = matrix()) {
  x_inverted <- NULL
  
  set <- function(y) {
    ## Assign the parameter matrix y to variable x so that it is accesssible to
    ## the other functions defined in makeCacheMatrix
    x <<- y
    
    ## Assigned the variable x_inverted the value NULL until it is set by 
    ## calling the set_inverted function below.  This means that a previously
    ## calculated inverse matrix will be set to NULL if the original matrix
    ## is changed.
    x_inverted <<- NULL
  }
  
  get <- function() {
    ## Return matrix x
    x
  }
  
  set_inverted <- function(y_inverted) {
    ## Assign the parameter matrix y_inverted (which result from inverting 
    ## matrix x) to the variable x_inverted so that it is accessible to the
    ## other functions defined in makeCacheMatrix
    x_inverted <<- y_inverted
  }
  
  get_inverted <- function() {
    ## Return matrix x_inverted, which should be the inverse of matrix x.
    x_inverted
  }
  
  # Return a list containing the functions defined in the makeCacheMatrix
  # function.  Due to lexical scoping, the variables x and x_inverted are in
  # in scope for these functions.
  list (set = set, get = get, set_inverted = set_inverted, get_inverted = get_inverted)
}

## cacheSolve:
## This function returns a matrix that is the inverse of the matrix
## cahced by the environment in which the  parameter 'x' is defined.   

## 'x' is the list of functions returned by the function makeCacheMatrix.

## If the inverse of the matrix orginally passed to makeCacheMatrix has been
## cached and that matrix is unchanged, then the cached inverse matrix is returned.
## Otherwise, the inverse is caclculated using the solve() function and 
## cached before being returned.
cacheSolve <- function(x, ...) {
  ## Get locacl coppy of inverse matrix
  x_inverted <- x$get_inverted()
  
  ## Determine whether the inverse has previously been computed.
  if (!is.null(x_inverted)) {
    message("getting cached data")
    return(x_inverted)
  }
  
  ## If the original matrix was changed or if the inverted matrix has not
  ## yet been calculated, calculate it using the solve() function, cache 
  ## the inverse matrix using x$set_inverted() and return the inverse matrix.
  
  ## Note: don't create a local representation of the matrix named x because
  # it will overwrite the parameter x, which is actually a list of functions.
  x_inverted <- solve(x$get())
  x$set_inverted(x_inverted)
  x_inverted
}
