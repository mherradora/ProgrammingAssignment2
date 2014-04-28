## Put comments here that give an overall description of what your
## functions do
## The functions take a n x n matrix, create a new object with the inicial matrix and the cache of the inverse matrix

#####
## Short comment describing this function:
## This function creates a copy of an n x n matrix with four functions:
## 1. set: to define the value of the matrix
## 2. get: to return the value of the matrix
## 3. setsolve: to keep like in the cache the value of the inverse matrix
## 4. getsolve: to return the value of the inverse matrix that has been kept like in a cache

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#####
## Short comment describing this function:
## This function evaluate if the inverse matrix of a n x n matrix had been already calculate.
## If this is true then the function get the previous calculate inverse matrix
## If this is false then the function calculate the inverse matrix and set it like in a cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

#####
#	Elements to test an n x n matrix

#n <- 3
#tmp <- matrix(rnorm(n*n),n,n)
#tmp1 <- makeCacheMatrix(tmp)
#tmp2 <- cacheSolve(tmp1)
#round(tmp %*% tmp2, 3)
