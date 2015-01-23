
# This object-like function caches square matrix and its inverse. The function accepts one argument: 
# a square matrix and returns a vector of following functions: matrix  (1)getter(getMat) and (2)setter(setMat) 
# and inversre matrix (3)getter(getInvMat) and (4)setter(setInvMat)
# Assertion tests: (1) - The function checks if the inserted argument is of a matrix type (2)- If the 
# matrix is indeed square
# Note: The function doesn't calculate the inverse of the matrix, however it is possible to set the value
# of the inverse matrix in the cache with the setInvMat function 


makeCacheMatrix <- function(m = matrix()){
  
  # auxiliary function: tests whether iserted matrix is square
  issquare <- function(mat){if(nrow(mat) == ncol(mat)){TRUE}else{FALSE}}
  
  # test assertion #1: accepted argument should be matrix
  if (class(m) != "matrix"){return(print(paste("inserted argument should be matrix, not:", class(m))))}
  
  # test assertion #2: accepted matrix should be square
  if(!issquare(m)){return(print("inserted matrix is non-square"))}
  
  
  invMat = matrix()
  
  # matrix getter: func. returns cached matrix if exsited, else returns NA
  getMat <- function(){if(all(is.na(m))){return(NA)}else{m}}
  
  # matrix setter: func. accepts matrix if squared, else returns error message
  setMat <- function(n){if(!issquare(n)){return("inserted matrix is non-square")}else{m <<- n}}
  
  # inverse matrix getter: func. returns cached matrix if exsited, else returns NA
  getInvMat <- function(){
    if(all(!is.na(invMat))){invMat}
    else{return(NA)}
  } 
  
  
  # inverse matrix setter: func. accepts inverse matrix if squared, else returns error message
  setInvMat <- function(someInvMat){if(!issquare(someInvMat)){return("inserted matrix is non-square")}else{invMat <<- someInvMat}}
  
  # the main func. returns vector of functions
  list(getMat = getMat, setMat = setMat, getInvMat = getInvMat, setInvMat = setInvMat)
  
}


# This function extracts/calculates the inverse matrix from the object-like makeCacheMatrix function.
# The function accepts one argument: makeCacheMatrix function, and returns the inverse of cached matrix, 
# either by calculation or by retrieval.
# Note 1: If there is no matrix cached, the function will return an error message
# Note 2: If there are both matrix and the inverse cached, but the inverse matrix isn't the inverse of 
# the cached matrix, the function will calculate and return the inverse of the cached matrix


cacheSolve <- function(obj, ...){
  
  # if there is no cached inverse matrix:
  if(all(is.na(obj$getInvMat()))){
    
    # and there is a cached matrix: calculate and return inverse matrix
    if(!all(is.na(obj$getMat()))){return(solve(obj$getMat()))}
    
    # and there is no cached matrix: return error message 
    else{return(print("there is no matrix in the cache to inverse"))}
  }
  
  # if there is a cached inverse matrix:
  else{
    
    # and there is a cached matrix: 
    if(!all(is.null(obj$getMat()))){
      
      # and the matrix has not changed: return inverse matrix
      if(identical(obj$getInvMat(),solve(obj$getMat()))){return(obj$getInvMat())}
      
      # and the matrix has changed: calculate and return inverse matrix
      else{return(solve(obj$getMat()))}
    }
    # and there is no cached matrix:
    else{return(obj$getInvMat())}
  }
  
}  

