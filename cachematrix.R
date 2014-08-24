
## Given a matrix, return the inverse of that matrix.
##(openly using the example function as a base)

## To only calculate a matrix inverse when solution not already stored in the cache
## will be doing this in two parts; designed to be called in tandem.
## i.e. to call the two functions, one could nest them as: cacheSolve(makeCacheMatrix(x)) where x is a matrix.

#### 1) makeCacheMatrix will create a custom object that will then be read 
#### by cacheSolve.

#### 2) cacheSolve will read in the custom object; and if the inverse has not yet
#### been solved and stored to cache, will then (and only then) calculate the inverse (to save time!)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL  
  set <- function(y) {  ##creating a function that we will use below, sets hard values
    x <<- y  ## setting this as a global value using lexagraphical scoping
    inverse <<- NULL  
  }
  get <- function() x  ## have we already gotten the value, if so, record it.
  setinverse <- function(solve) inverse <<- inverse  ## now setting this as a global value using lexagraphical scoping
  getinverse <- function() inverse  ##this is the element that cacheSolve will be looking for!
  list(set = set, get = get,  ## this is the handoff element we will pass to cacheSolve
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse.
    
  inverse <- x$getinverse() ##pulling out just the element of greatest interest
  if(!is.null(inverse)) {  #if we already have the inverse for this matrix; i.e., it's not null then...
    message("getting inverse of matrix from the cache") ##we print a message and return the inverse we already found
    return(inverse)
  }
  data <- x$get()   ##else, if we don't have it cached, load the data we stored during makeCacheMatrix
  inverse <- solve(data, ...)  #and use that stored data to find the matrix inverse explictly.
  x$setinverse(inverse)
  inverse  ##and we return the inverse we just calculated (because it was not in the cache)
}