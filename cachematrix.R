## These functions work together to cache a Matrix input as well as it's inverse value. The theory
## behind storing the variables in the cache is easy accessing the values if necessary without having
## to store them within the current environment.



## The first funcion makeCacheMatrix can be intiazled similarly to the way an object of a class in 
##other programming languages might be initialized, once the object is created, it is possible 
##to access the functions within the class to get and set a matrix in a cache (alt environment) 
##as well as get and set the matrix's inverse. 

makeCacheMatrix<- function(x=matrix()){
  
  inv <- NULL
  set <- function(y){
    
    x<<-y
    inv<<-NULL
    
  }
  
  get <- function() x
  setInverse <- function(inverse) inv<<-inverse
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)  
  
  
  
}


## The second function accepts an object of the the makeCahceMatrix variable type and accesses the getinverse
## function to first check for existence, if the value checked is NULL, the cacheSolve function solves
## the inverse of the matrix. The assumption is that the only type of matrix fed into the function 
## wil be invertible.

cacheSolve<- function(x,...){
  
  inv <- x$getInverse()
  if(!is.null(inv)){
    
    message("getting cached data")
    return(inv)      
    
  }
  
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
  
  
}
