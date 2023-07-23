## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { # x is an empty matrix
 i<- NULL # initialisng the inverse property
 get<- function() x # This function returns the cached matrix x.
 set<- function(y) { # seting values of the matrix x
       x<<- y
       i<<- NULL 
       # When the matrix x is updated, the inverse i is reset to NULL to ensure the cached inverse is not outdated.

 }

 geti<- function() i
 seti<- function(inverse) {
      i<<- inverse
 }
 return(list(
      set = set,
      get = get,
      getinverse = geti,
      setinverse = seti
 ))
}

cacheSolve <- function(x, ...) { #It takes a parameter x, which is expected to be an object created by makeCacheMatrix
      inverse<- x$getinverse()
      if (!is.null(inverse)) {
            return(inverse) # It first checks if the inverse of the matrix is already cached by calling x$getinverse(). If the inverse exists (!is.null(inverse)), it returns the cached inverse directly.
      }  
      m<- solve(x$get())
      x$setinverse(m) #If the inverse is not cached, it calculates the inverse of the matrix using the solve function and caches the result using x$setinverse(m), where m is the calculated inverse.
}
