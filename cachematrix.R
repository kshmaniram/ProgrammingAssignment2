## Put comments here that give an overall description of what your
## functions do

## 
# This function creates special vector that contain list of following functions:
#  * getMatrix - This function returns the matric from the cache
#  * setMatrix - This function puts the given matrix to the cache thru environment variable 
#  * getInverseMatrix - This function retrieves the inverse of the given matrix
#  * setInverseMatrix - This function sets the inverse of the matrix to the cache thru environment variable
#
makeCacheMatrix <- function(x = matrix()) {
   # Variable to assign inverse matrix
   invm <- NULL;
   
   # Set the given matrix to the environment variable
   setMatrix<- function(y){
      x <<- y;
      invm <<- NULL;
   }
   
   # Get matrix from cache
   getMatrix <- function() x;
   
   # Set the inverse of the matrix to the cache
   setInverseMatrix <- function(invmatrix) invm <<- invmatrix;
   
   # Get the inverse of the matrix from the cache
   getInverseMatrix <- function() invm;
   
   # Create list of functions
   list(setMatrix = setMatrix, getMatrix = getMatrix,
        setInverseMatrix = setInverseMatrix,
        getInverseMatrix = getInverseMatrix);
}


## 
# Find the inverse of the matrix and set it to the cache
#
#

cacheSolve <- function(x, ...) {
   
   # Check if the inverse of the matrix is available in cache
   inv <- x$getInverseMatrix();
      
   # If yes,get it from cache
   if(!is.null(inv)){
      message("Getting matrix from cache.");     
      return(inv);
   }
   
   # Get the matrix from the cache
   mat <- x$getMatrix();
   
   # Inverse the matrix
   inv <- solve(mat);
   
   # Set inverse of the matrix to the cache
   x$setInverseMatrix(inv);
   
   # Return the inverse of matrix
   inv;
   
}
