## Courseara R Programming
## Assignment 2

## makeCacheMatrix creates a new matrix using specified arguements, then
## calculates the inversematrix and caches it

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

##  cacheSolve checks to see if the inverse matrix has been calculated and stored
##  if the invers has been stored it retrieves it, then prints it
##  if it has not been stored the function calculates teh inverse and then prints it

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

## Test code to check results; uncomment and run to confirm

## mat <- matrix(1:4, 2,2)
## mat2 <- makeCacheMatrix(mat)
## mat2
## cacheSolve(makeCacheMatrix(mat))