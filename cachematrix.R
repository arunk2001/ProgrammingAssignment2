## Function to cache the inverse of an invertible matrix and to display the 
## inverted cache matrix using another function.

## A function that reads in a matrix and makes a list containing the function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
    }
  get <- function() x
  setcache <- function(solve) inverse <<- solve
  getcache <- function() inverse
  list(set = set, get = get,
       setcache= setcache,
       getcache = getcache)
}


## A function to calculate the inverse of the matrix using the solve() function

cacheSolve <- function(x, ...) {
  inverse <- x$getcache()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setcache(inverse <- x$getcache())
             if(!is.null(inverse)) {
               message("getting cached data")
               return(inverse)
             }
             data <- x$get()
             inverse <- solve(data, ...)
             x$setcache(inverse)
             inverse
        ## 'inverse' is the matrix that is the inverse of 'x'
}
