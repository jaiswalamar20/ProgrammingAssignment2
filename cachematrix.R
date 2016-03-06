##function takes input as a matrix in argument x
##We then Initialise the inverse object as NULL
## set the varibale x with new matrix if not already found and i should be NUll
## get function gets the matrix to be inverted from x
## setinverse sets the inverse of the matrix calculated by cacheSolve in i
## getinverse gives the inverse of the matrix saved into i in earlier step
makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL                         
  set <- function(y) {              
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(Solve) i <<- Solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Write a short comment describing this function
## In cacheSolve function we see if the inverse of matrix x is already present in the cache or not
## if so then we get its inverse into i and return i
## if the inverse is not alrady present then we put the matirx into data and calculate its inverse
## and save the inverse into i that is stored in the makecacheMatrix
## and we set the setinverse variable.
## last;y we return the inverse of the matrix calculated by our function.
cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
