## These two function are set to chache the inverse of a matrix

## This first fuction creates a special matrix which is really a list containing a function to 
## 1) set the value of the matrix, 2) get the value of the matrix 
## 3) set the value of the inverse matrix 4) get the value of the inverse matrix

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

## This second function calculate the inverse of the special matrix created with 
## the above function. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the 
## cache via the setmatrix function.

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
## Return a matrix that is the inverse of 'x'
a<- makeCacheMatrix()
a$set(matrix(1:4,2,2))
cacheSolve(a)