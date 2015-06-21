##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x= matrix ()) {
m<- null

##set the matrix
set<- function(y) {
x<<- y
m<<- null }

##get the matrix
get <- function() { x }

##set inverse of the matrix
setinverse <- function(inverse) { m <<- inverse }

##get inverse of the matrix
getinverse <- function()  { m }

## list of all argument
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
cachesolve <- function(x, ...) {
 
##return of the inverse matrix if it already exists
m <- x$getinverse()
if(!is.null(m)) {
message("getting cached data")
return(m)
}

##get the matrix from dataset
data <- x$get()

##  perform inverse operation for non-singluar sq matrix
m <- solve(data, ...)

##set the inverse of the object
x$setinverse(m)
       
## return the matrix
m
}