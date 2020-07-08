
# create matrix
makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL # set inverted as null
  set <- function(y){
    x <<- y #scope x
    inverted <<- NULL #scope inverted as null
  }
  get <- function() x
  setinverted <- function(solveMatrix) inverted <<- solveMatrix
  getinverted <- function() inverted
  list(set = set, 
       get = get, setinverted = setinverted, 
       getinverted = getinverted)
}

cacheSolve <- function(x, ...) {

  inverted <- x$getinverted()
  if(!is.null(inverted)){ #check for null cells
    message("getting data")
    return(inverted)
  }
  data <- x$get()
  inverted <- solve(data)
  x$setinverted(inverted)
  inverted      
}

