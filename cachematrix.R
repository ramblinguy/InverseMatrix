## Takes a matrix, gets its inverse, and stores it in cache

## Takes a matrix and gets ready to cache it
makeCacheMatrix <- function(x = matrix()){
  i <- NULL #declare the variable that the inverse is going to be stored in
  set <- function(y) { #I believe that this will be used to set a new matrix and reset the inverse
    x <<- y
    i <<- NULL
  }
  get <- function() x #this function just returns the matrix that was passed originally
  setinv <- function(inv) i <<- inv #this stores the inverse so that it can be retrieved later
  getinv <- function() i #this returns the inverse for caching purposes
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) #I think this is necessary to return the list of functions that were created.
}

## Calculates an inverse and stores it in cache. 
cacheInv <- function(x, ...) {
  i <- x$getinv() #checks to see if i in makeCacheMatrix is populated. If it is, i is returned and calculations are skipped.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get() #storing the matrix passed to makeCacheMatrix in data
  i <- solve(data, ...) #actually getting the inverse
  x$setinv(i) #storing the inverse in the cache
  i
}