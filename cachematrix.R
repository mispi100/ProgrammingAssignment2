## The first function reads in a matrix and transforms it into a list object, which contains different functions, like to get the matrix (write out), to set a matrix (write in) and to get and set the inverse matrix. 
## The second function reads in the output of the first function and calculates the inverse matrix. This matrix is now stored in the list object. 

## Calculates special matrix object

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function (y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of a special matrix object and returns it

cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

