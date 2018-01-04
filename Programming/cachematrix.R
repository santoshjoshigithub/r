## Matrix inversion is usually a costly computation and it would be more effecient if we cache the computation. 
## Following functions illustrate how we can achieve caching of a costly computation using lexical scoping.

## makeCacheMatrix is a constructor function. There are 4 functions inside it. Please refer comments for each 
## function below.

  makeCacheMatrix <- function(x = matrix()) 
    {
      ## Initialize i as NULL. This variable will store inversion of the "invertible" matrix.    
      i <- NULL
      ## setMatrix initialize the value of the "invertible" matrix.
      setMatrix <- function(y) 
      {
        ## A matrix is passed to the formal argument "y" and then it's stored to "x" in the parent variable using
        ## super assignment operator "<<-".
        x <<- y
        ## i is initiated to NULL to flush out the old value it stores in the parent environment.
        i <<- NULL
      }
      
      ## getMatrix function return a matrix whose inversion is computed.
      getMatrix <- function() x
      ## setInverseOfMatrix function sets the value of inversion of the Matrix in the parent environment.
      ## The value of iom is actually computed in the "cacheSolve" function and then passed to this function. 
      setInverseOfMatrix <- function (iom) 
      {	
        ## i is initiated to iom in the parent environment.
        i <<- iom			
      }	
      ## getInverseOfMatrix function simply return the inversion of the Matrix.
      getInverseOfMatrix <- function () i
      ## Following list is used to expose above function to the external environment so that they could
      ## be accessed and used for computation.
      list (setMatrix  = setMatrix, getMatrix = getMatrix, setInverseOfMatrix = setInverseOfMatrix, getInverseOfMatrix = getInverseOfMatrix)
    }
## cacheSolve function is basically used to compute the inversion of the matrix.
  cacheSolve <- function(x, ...) 
    {
      ## Following code snippet first checks if the inversion of matrix is already computed. If it is then
      ## it returns the already computed value and hence making the computation more effecient.
      iom <- x$getInverseOfMatrix()
      if(!is.null(iom))
      {
        message("You are lucky, inverse of the matrix is in the Cache!!")
        return(iom)
      }
      ## Following block of code computes the inversion of a matrix using "solve" built in R function and then 
      ## pass it to the makeCacheMatrix using formal argument "x" to store it's value for caching.
      else 
      {
        data <- x$getMatrix()
        iom <- solve(data, ...)
        x$setInverseOfMatrix(iom)
        return(iom)
      }
    }
