
##makeCacheMatrix: This function creates a special 
##"matrix" object that can cache its inverse

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get<- function() x
  setinverse<- function(inverse) inv <<- inverse
  getinverse<- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##cacheSolve: This function computes the inverse of the special
##"matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  Inverse<- function(A)
  {
    I<- matrix(data = NA, nrow = nrow(A), ncol = ncol(A))
    for(i in 1:nrow(A))
    {
      for(j in 1:ncol(A))
      {
        I[j,i]<- (-1)^(i+j)*det(A[-i,-j])/det(A)
      }
    }
    invisible(I)
  }
  ##Inverse() function creates the inverse of the matrix, this is used instead 
  ## of solve() function
  inv<- Inverse(data)
  x$setinverse(inv)
  inv
}

##Example
##>G<- matrix(c(1,0,1,2,4,0,3,5,6),3,3)
##>b<- makeCacheMatrix(G)
##>cacheSolve(b)
#[,1]        [,2]        [,3]
#[1,]  1.0909091 -0.54545455 -0.09090909
#[2,]  0.2272727  0.13636364 -0.22727273
#[3,] -0.1818182  0.09090909  0.18181818
