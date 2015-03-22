## makeCacheMatrix() takes a matrix and stores it to an object 'x' within the environment of the function,
## then creates an object 'm' where the inverse of the matrix can be stored and retrieved.

makeCacheMatrix <- function(x = matrix()) {             
  m <- NULL                                
  set <- function(y) {                          ## set() is a function within makeCacheMatrix()
    x <<- y                                     ## stores the input to set() as an object 'x'
    m <<- NULL                                  ## creates an empty object 'm'
    
    get <- function() x                           ## get() returns 'x'
    setinverse <- function(solve) m <<- solve     ## setinverse() uses solve() to return the inverse of matrix 'x' to 'm'
    getinverse <- function() m                    ## getinverse() returns 'm'
    
    list(set = set, get = get,                    ## makeCacheMatrix() returns a list containing the values of set, get, setinverse, and getinverse
         setinverse = setinverse,              
         getinverse = getinverse)
  }


## cacheSolve() looks to see if the solution to a matrix has been cached with makeCacheMatrix() and 
## either returns the cached solution, or solves and caches the input matrix for future use

cacheSolve <- function(x, ...) {                 
  m <- x$getinverse()                                ## runs getinverse()  
  if(!is.null(m)) {                                  ## looks up & returns stored inverse of matrix
    message("getting cached data")
    return(m)
  }
  data <- x$get()                             
  m <- solve(data, ...)                             ## solves matrix
  x$setinverse(m)                                   ## caches inverse of matrix for future use
  m                                                 ## returns inverse of matrix 
}
