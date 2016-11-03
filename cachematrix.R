## This "magic" function will input a matrix and store it in the parent 
## environment in the correct format for lexical scoping

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
}

## mat = matrix(1:4,2,2)
## myMatrix = makeCacheMatrix(mat)
## cacheSolve(myMatrix)
## This function will execute the actual calling functions and use solve to calculate the inverse matrix solution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
  
}
## setwd("C:\\Users\\Jeff\\Documents\\GitHub\\ProgrammingAssignment2")
