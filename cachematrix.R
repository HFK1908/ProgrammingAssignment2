# The result of MakeCacheMatrix is a list. This list contains the following :
# 1. setting the value of a specified matrix.
# 2. getting the value of a specified matrix.
# 3. setting the value of inverse of a specified matrix.
# 4. getting the value of inverse of a specified matrix.

MakeCacheMatrix <- function(x = matrix()) 
{
  inv_a <- NULL
  set <- function(y) {
    x <<- y
    inv_a <<- NULL
  }
  get <- function() x
  setinverse <- function(inv_b) inv_a <<- inv_b
  getinverse <- function() inv_a
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The function CacheSolve gives the inverse of a specified matrix. It does the following things :
# 1. It checks if an inverse has already been computed.
# 2. If the answer to 1 is yes, it doesn't compute again but gives the same result as the previous computation.
# 3. If the answer to 1 is no, it does compute the inverse using the setinverse-function specified above.
# 4. The function gives an error if the specified matrix can't be inversed.

SolveCache <- function(x, ...) 
{
  inv_a <- x$getinverse()
  if(!is.null(inv_a)) {
    message("getting cached data")
    return(inv_a)
  }
  data <- x$get()
  inv_a <- solve(data)
  x$setinverse(inv_a)
  inv_a
}

# To test if the above functions work, we can use a 3x3 matrix. The results below show that the functions work : 
# 1. The m$get shows the matrix we will use to test.
# 2. The first run of SolveCache computes the inversed matrix.
# 3. The second run of SolveCache gives the message "getting cached data" and returns the inversed matrix from the cache.
# 4. The fourth segment creates a 3x2 matrix which causes CacheSolve to error out.

x <- rbind(c(10, 14, 13), c(15, 11, 16), c(19, 18, 17))
m <- MakeCacheMatrix(x)
m$get()
# [,1] [,2] [,3]
# [1,]   10   14   13
# [2,]   15   11   16
# [3,]   19   18   17
SolveCache(m)
# [,1]         [,2]        [,3]
# [1,] -0.2153518 -0.008528785  0.17270789
# [2,]  0.1044776 -0.164179104  0.07462687
# [3,]  0.1300640  0.183368870 -0.21321962
SolveCache(m)
# getting cached data
# [,1]         [,2]        [,3]
# [1,] -0.2153518 -0.008528785  0.17270789
# [2,]  0.1044776 -0.164179104  0.07462687
# [3,]  0.1300640  0.183368870 -0.21321962
x <- rbind(c(10, 14), c(15, 11), c(19,18))
m <- MakeCacheMatrix(x)
CacheSolve(m)
# Error in solve.default(data) : 'a' (3 x 2) must be square 
