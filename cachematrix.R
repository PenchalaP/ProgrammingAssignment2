## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      
  invers <- NULL
  # set the value of the matrix
  set <- function(y){
    x <<- y
    invers <<- NULL
  }
   # get the value of the matrix    
  get <- function() x
   # set the value of the inverse   
  setInverse <- function(solveMatrix) invers <<- solveMatrix
   # get the value of the inverse   
  getInverse <- function() invers
      
  list(set = set, get =get, setInverse =setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # check to see if the inverse is already there    
   
   invers <- x$getInverse()
    # check if the inverse is there, then return it  
  if(!is.null(invers)){
    message("************* Cached Data **********************")
    return (invers)
  }
  # otherwise, solve the matrix and produce the inverse    
  data <- x$get()
  invers <- solve(data)
  x$setInverse(invers)
  invers    
        
}
