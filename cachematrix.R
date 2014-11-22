##The pair of functions below is to cache the inverse of a matrix

## create a function to store a special matrix used to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function(){x}
  setinverse<-function(solve) {inverse<<-solve}
  getinverse<-function(){inverse}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## calculate the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse<-x$getinverse()
  if(!is.null(inverse)){
    message("getting cached inverse")
    return(inverse)
    }
  data<-x$get()
  inverse<-solve(data,...)
  x$setinverse(inverse)
  inverse
}
