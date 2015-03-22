# makeCacheMatrix(matrix) produces a list
# this list incapsulates accessor methods for getting and setting values in a particular environment
makeCacheMatrix <- function(x = matrix()) 
{
  #clean cached value of matrix inversion 
  solveValue <- NULL

  # setter 
  setValue <- function(matrixValue)
  {
    #store new value
    x <<- matrixValue
    #clean cached value of matrix inversion in outer scope
    solveValue <<- NULL
  }
  # getter 
  getValue <- function()
  { 
    x
  }
  setSolveResult <- function(val)
  {
    solveValue <<- val
  }
  getSolveResult <- function()
  {
    solveValue
  }
  list( set = setValue, get = getValue, setResult = setSolveResult, getResult = getSolveResult)
}


## This functions extends standard implementation of solve (matrix inversion) by providing ability to cache inversion results in an intermediate store

cacheSolve <- function(x, ...) 
{
  result <- x$getResult()
  if(is.null(result))
  {
    result <- solve(x$get())
    x$setResult(result)
  }
  result
}
