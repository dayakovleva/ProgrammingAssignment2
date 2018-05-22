#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
#rather than compute it repeatedly. The following two functions are to provide the solution to matrix inverse. 

#makeCacheMatrix() creates a special "matrix" object that can cache its inverse. 
#It contains a function to:
# 1. set the value of matrix
# 2. get the value of matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y){
    x <<- y
    cache <<- NULL
  }
  get <- function() x 
  setMatrix <- function(inverse) cache <<- inverse
  getInverse <- function() cache
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


#cacheSolve() calculates the inverse of the matrix from makeCacheMatrix (it is assumed that the matrix is always invertible). 
#It checks if the inverted matrix exists in cache. It it is already in cache, the algorithms skips it, 
#otherwise the inverted matrix is calculated and its value is set in cache by setMatrix 

cacheSolve <- function(x, ...) {
  cache <- x$getInverse()
  if(!is.null(cache)){
    message("getting cached data")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data)
  x$setMatrix(cache)
  cache        
}

# For test:
#x = rbind(c(1, 2), c(-2, 1))
#m = makeCacheMatrix(x)
#m$get()
#     [,1] [,2]
#[1,]    1    2
#[2,]   -2    1

# No cache in the first run
#cacheSolve(m)
#     [,1] [,2]
#[1,]  0.2 -0.4
#[2,]  0.4  0.2

# Retrieving from the cache in the second run
#cacheSolve(m)
# getting cached data.
#     [,1] [,2]
#[1,]  0.2 -0.4
#[2,]  0.4  0.2
# > 
