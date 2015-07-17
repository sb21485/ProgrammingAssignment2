## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<- function(x = matrix()) {
  i <- NULL
  set.matrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  get.matrix <- function() x
  set.matrix <- function(inverse) i<<- inverse
  get.inverse <- function() i
  list(set.matrix = set.matrix, get.matrix = get.matrix,
       set.matrix = set.matrix,
       get.inverse = get.inverse)
}

cacheSolve <- function(x, ...) {
  
  a<-nrow(x$get.matrix())
  b<-ncol(x$get.matrix())
  if(a==b) print("good to go")
  else stop("enter a valid square matrix")
  
  #if (nrow(cm) = ncol(cm)) print("inverse can be created")
  
  i <- x$get.inverse()
  if(!is.null(i)) 
   {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get.matrix()
  i <- solve(data, ...)
  x$set.matrix(i)
  i
}
