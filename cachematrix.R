## makeCacheMatrix creates a matrix object that catches it's inverse

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y){
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set,get= get,setinverse = setinverse,getinverse=getinverse)
}


## catcheSolve computes the inverse of the matrix obtained from x by first checking if the matrix has
## been computed previosuly and if not, it uses the Solve() function to compute the matrix inverse

cacheSolve <- function(x, ...) {
        I <- x$getinverse()
        if(!is.null(I)){
          message("getting cached data")
          return(I)
        }
        data <- x$get()
        I <- solve(data)
        x$setinverse(I)
        I
}
