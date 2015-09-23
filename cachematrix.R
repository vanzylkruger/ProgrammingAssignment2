## Write a short comment describing this function
## To use this function you need to assign the function to a variable in the console
## i.e. x <- makeCacheMatrix()
## You can then assign a matrix using the set function i.e. x$set(matrix(1:4,2,2))
## Both the matrix and the inverse matrix is instantiated globally through the <<- assignment

makeCacheMatrix <- function(x = matrix()) {
  iM <- NULL
  set <- function(y) {
    x <<- y
    iM <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) iM <<- solve
  getInverseMatrix <- function() iM
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
## The function receives x and calls the getInverseMatrix function
## If the return value of the inverse matrix is null it uses the previously cached value
## It sets data equal to the return matrix from the get function
## iM is inversed through the solve function
## Finally the inverse matrix is set globally through the setInverseMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iM <- x$getInverseMatrix()
  if(!is.null(iM)){
      message("getting cahced data")
      return(iM)
  }
  data <- x$get()
  iM <- solve(data)
  x$setInverseMatrix(iM, ...)
  iM
}
