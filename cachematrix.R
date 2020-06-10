## Hello, I`m Alver. From the backbone, I created a function that calculates the inverse of a matrix. 
## My function takes a Matrix, transform it into a data frame and get the cache of it. Then, the second function calculates the inverse. 


MyMatrix <- matrix(c(1,7,17,33),2,2)
Ma2<-matrix(c(1,2,3,4), 2, 2)
q<-as.data.frame(MyMatrix)
p<-as.data.frame(Ma2)


## This is my function. I have to use data.frame for each matrix because I had an error with "$".

makeCacheMatrix <- function(x = data.frame()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x 
  setinverse <- function(inverse) m <<- solve(x)
  getinverse <- function() m  
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This is almost the same backbone. 

cachesolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

### Testing my function!!! 

cachep<-makeCacheMatrix(p)  
cachesolve(cachep)

cacheq<-makeCacheMatrix(q)
cachesolve(cacheq)

