## Both of these functions overall attempt to save time when calculating inverses of matrixs
## If the inverse of a matrix has already been calculated, then it will pull that previously
##calculated answer rather than recomputing.

## This functions stores our special matrix and holds our equations to get, set, getinverse, and setinverse. 
makeMatrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
  
}

##This function will return the inverse matrix of x in one of two ways. It will either pull from
##previously stored data or it will compute the inverse and store it for later use. 
cachesolve <- function(x,...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data, please hold")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv ##returns the inverse matrix
}

##this was just testing to ensure everything came out correctly. Did have to make some changes but it seems
##to be working correctly now. 
my_first <- makeMatrix(matrix(1:4,2,2))
my_first$get()
my_first$getinverse()
cachesolve(my_first)
cachesolve(my_first)

