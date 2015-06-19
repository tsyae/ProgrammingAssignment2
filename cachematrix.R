## Put comments here that give an overall description of what your
## functions do




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

## cacheSolve:
## This function returns a matrix that is the inverse of the matrix
## cahced by the environment in which the  parameter 'x' is defined.   

## 'x' is the list of functions returned by the function makeCacheMatrix.

## If the inverse of the matrix orginally passed to makeCacheMatrix has been
## cached and matrix is x is unchanged, then the cached inverse of x is returned.
## Otherwise, the inverse of matrix x is caclculated using the solve() function and 
## the inverse of matrix x is cached before it is returned.
cacheSolve <- function(x, ...) {
  ## Get locacl coppies of the matrix x and its inverse
  x <- x$get()
  x_inverted <- x$get_inverted()
  
  ## Determine whether the inverse of matrix x has previously been computed.
  if (!is.null(x_inverted)) {
    
    ## After establishing that the inverse of matrix x has been previously computed
    ## (it was not NULL), we next determine whether the original matrix x was
    ## changed since x_inverted was calculated. 
    
    ## My linear algebra is rusty, but I believe multiplying a matrix by its inverse
    ## gives the identity matrix of the same rank.  If so, x multiplied by x_inverted
    ## will result in a product that is the identity matrix for matrix x.  If this
    ## is true, assume matrix x has not been changed since x_inverted was set and
    ## return x_inverted.
    product_matrix <- x %*% x_inverted
    identity_matrix <- diag(nrow(x))
    if (all(product_matrix == idenity_matrix)) {
      message("getting cached data")
      return(x_inverted)
    }
  }
  ## If x has been changed since x_inverted was calculated or if x_inverted has not
  ## yet been calculated, calculate it using the solve() function, cache x_inverted
  ## using x$set_inverted(), and finally, return x_inverted.
  x_inverted <- solve(x)
  x$set_inverted(x_inverted)
  x_inverted
}
