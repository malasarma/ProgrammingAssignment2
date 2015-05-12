## Here is an example on how to invoke the functions and the results:
## > a <- makeCacheMatrix()
## > a$set(matrix(c(1, -0.25, -0.25, 1), 2, 2))
## > a$get()
##   [,1]  [,2]
##   [1,]  1.00 -0.25
##   [2,] -0.25  1.00
## > a$getInvM()
##   NULL
## > cacheSolve(a)
##   [,1]      [,2]
##   [1,] 1.0666667 0.2666667
##   [2,] 0.2666667 1.0666667
## > cacheSolve(a)
##   getting cached data
##   [,1]      [,2]
##   [1,] 1.0666667 0.2666667
##   [2,] 0.2666667 1.0666667
## > a$set(matrix(c(10, 20, 20, 10), 2, 2))
## > a$get()
##   [,1] [,2]
##   [1,]   10   20
##   [2,]   20   10

## makeCacheMatrix creates and returs a list of funtions
## used by cacheSolve to get/set an inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
## stores the cached matrix
## initialized to NULL
  inv <- NULL
  
## create the matrix the working environment, 
## setting the inverse to NULL

  set <- function(y) {
          x <<- y
          inv <<- NULL
  }

## function to retrieve the matrix
  get <- function() x

## store the inverted matrix in cache
  setInvM <- function(invm) inv <<- invm

## retrieve the inverted matrix from cache
  getInvM <- function() inv

## create a list of the above functions and return to working env

  list(set = set, get = get, setInvM = setInvM, getInvM = getInvM)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and its inverted value
## is stored in cache. If it exists, it retrieves the previously calculated
## inverted matrix.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInvM()
        
        ## if inv is not NULL, the inverse matrix already exists, does not need to be evaluated
        ## and can be retrieved from cache.
        ## Return the inverse matrix.
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## if inv is NULL, the inverse matrix needs to be evaluated
        data <- x$get()
        inv <- solve(data, ...)
        
        ## Save the inverse matrix in the cache
        x$setInvM(inv)
        
        ## return the inverse matrix
        inv
}
