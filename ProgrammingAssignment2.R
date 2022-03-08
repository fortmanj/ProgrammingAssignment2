## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
##There are two functions makeCacheMatrix,makeCacheMatrix
##makeCacheMatrix consists of set,get,setinv, getinv
##library(Mass) is used to calculate inverse for non squared as well as squared matricies

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL          #initializing inverve as NULL
  set <- function(y){
                      x <<- y
                      inv <<-NULL
  }
  get <- function()x   #function to get matrix x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function(){
                        inver<-ginv(x)
                        inver%*%(x)      #function to obtain inverse of the matrix
                        }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##This is used to get the cache data

cacheSolve <- function(x,...) ##gets cache data
  {
  inv<-x$getinv()
  if(!is.null(inv)){            #checking whether inverse if Null
      message("getting chached data!")
      return(inv)               #returns inverse value
  }
  data<-x$get()
  inv<-solve(data,...)   #calculates inverse value
  x$setinv(inv)
  inv ## Return a matrix that is the inverse of 'x'
}

##testing code: 

> f<-makeCacheMatrix(matrix(1:8,2,4))
> f$get()
[,1] [,2] [,3] [,4]
[1,]    1    3    5    7
[2,]    2    4    6    8

> f$getinv()
[,1] [,2] [,3] [,4]
[1,]  0.7  0.4  0.1 -0.2
[2,]  0.4  0.3  0.2  0.1
[3,]  0.1  0.2  0.3  0.4
[4,] -0.2  0.1  0.4  0.7

> cacheSolve(f)
getting chached data!
  [,1] [,2] [,3] [,4]
[1,]  0.7  0.4  0.1 -0.2
[2,]  0.4  0.3  0.2  0.1
[3,]  0.1  0.2  0.3  0.4
[4,] -0.2  0.1  0.4  0.7

  