## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  x_inverted <- NULL
  
  set <- function(y) {
    ## Assign the parameter matrix y to variable x so that it is accesssible to
    ## the other functions defined in makeCacheMatrix
    x <<- y
    
    ## Assigned the variable x_inverted the value NULL until it is set by 
    ## calling the set_inverted function below 
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
  list (
        set = set, 
        get = get, 
        set_inverted = set_inverted, 
        get_inverted = get_inverted
        )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
